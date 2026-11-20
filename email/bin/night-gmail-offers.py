#!/usr/bin/env python3
"""Read a bounded Gmail window and deliver qualified offers to Telegram."""

import argparse
import base64
import datetime as dt
from email.utils import parseaddr
import fcntl
from html import escape
from html.parser import HTMLParser
import json
import math
import os
from pathlib import Path
import re
import subprocess
import time
import unicodedata
from urllib.parse import quote, urlsplit
from uuid import uuid4


ROOT = Path(os.environ.get('NIGHT_GMAIL_HOME', Path.home() / '.local/share/night-gmail'))
STATE = ROOT / 'offer-scan'
DAY = 86400
OFFER_HISTORY_GRACE = dt.timedelta(days=7)
API = 'https://gmail.googleapis.com/gmail/v1/users/{}/messages'
PHOTO_LABEL = 'Product photo'
PHOTO_LINE = f'  ![{PHOTO_LABEL}]('


def gmail():
    from google.auth.transport.requests import AuthorizedSession
    from google.oauth2.credentials import Credentials

    path = ROOT / 'mail/gmail/.credentials.gmailieer.json'
    return AuthorizedSession(Credentials.from_authorized_user_file(path))


def request(session, path='', **params):
    account = json.loads((ROOT / 'mail/gmail/.gmailieer.json').read_text())['account']
    response = session.get(API.format(quote(account, safe='')) + path, params=params, timeout=30)
    if not response.ok:
        raise RuntimeError(f'Gmail API returned HTTP {response.status_code}')
    return response.json()


def save(state):
    temporary = STATE / 'state.tmp'
    temporary.write_text(json.dumps(state, ensure_ascii=False))
    temporary.replace(STATE / 'state.json')


def query_window(checkpoint, now):
    start = max(checkpoint if checkpoint is not None else now - DAY, now - 7 * DAY)
    after = max(start - 1, now - 7 * DAY)
    terms = (STATE / 'query').read_text().strip()
    if not terms:
        raise ValueError('Missing Gmail offer search terms')
    return start, f'{terms} after:{after} before:{now + 1}'


def fresh(timestamp):
    return timestamp >= time.time_ns() // 1_000_000 - 7 * DAY * 1000


def keepa_alert(headers):
    sender = next((h['value'] for h in headers if h['name'].lower() == 'from'), '')
    domain = parseaddr(sender)[1].rpartition('@')[2].casefold()
    return domain == 'keepa.com' or domain.endswith('.keepa.com')


def prune_history(state):
    cutoff = dt.datetime.now(dt.timezone.utc) - OFFER_HISTORY_GRACE
    if 'delivered_offers' in state:
        state['delivered_offers'] = [offer for offer in state['delivered_offers']
                                     if dt.datetime.fromisoformat(offer['expires']) > cutoff]


def fetch(state, probe=False):
    now = int(time.time())
    start, query = query_window(state.get('checkpoint'), now)
    candidates = {}
    with gmail() as session:
        args = {'q': query, 'maxResults': 500, 'fields': 'messages/id,nextPageToken,resultSizeEstimate'}
        while True:
            page = request(session, **args)
            for hit in page.get('messages', []):
                message_id = hit['id']
                details = request(session, '/' + message_id, format='metadata',
                                  metadataHeaders='From', fields='internalDate,payload/headers')
                received = int(details['internalDate'])
                if (start * 1000 <= received < now * 1000 and fresh(received)
                        and not keepa_alert(details.get('payload', {}).get('headers', []))):
                    candidates[message_id] = received
            if 'nextPageToken' not in page:
                break
            args['pageToken'] = page['nextPageToken']

    if probe:
        print(json.dumps({'candidates': len(candidates)}))
    elif candidates:
        prune_history(state)
        pending = {'token': uuid4().hex, 'through': now, 'candidates': candidates, 'opened': []}
        state['pending'] = pending
        save(state)
        print(json.dumps({'token': pending['token'], 'ids': list(candidates),
                          'delivered_offers': state.get('delivered_offers', [])}, ensure_ascii=False))
    else:
        finish_window(state, now)


class EmailHTML(HTMLParser):
    def __init__(self, images):
        super().__init__(convert_charrefs=True)
        self.text = []
        self.images = images
        self.hidden = None

    def handle_starttag(self, tag, attrs):
        if tag in {'style', 'script', 'template'}:
            self.hidden = tag
        if not self.hidden and tag in {'br', 'p', 'li', 'tr', 'div'}:
            self.text.append('\n')
        if self.hidden:
            return
        attrs = dict(attrs)
        link = attrs.get('href')
        if tag == 'a' and link and valid_link(link):
            self.text.append(f' [Link: {link}] ')
        if tag == 'img':
            url = public_image_url(attrs)
            if url and url not in self.images:
                self.images[url] = ' '.join((attrs.get('alt') or '').split())[:160]

    def handle_endtag(self, tag):
        if self.hidden == tag:
            self.hidden = None

    def handle_data(self, data):
        if not self.hidden:
            self.text.append(data)


def valid_link(link):
    try:
        url = urlsplit(link)
        return url.scheme in {'https', 'http'} and bool(url.hostname) and not url.username
    except ValueError:
        return False


def public_image_url(attrs):
    src = attrs.get('src') or ''
    # Product photos use public asset URLs. Do not embed personalized URLs,
    # tracking pixels, or addresses that the sender's browser must resolve.
    if len(src) > 1024 or not re.fullmatch(
        r'https://(?:[a-z0-9-]+\.)+[a-z]{2,}/[a-z0-9._~%/()-]+\.(?:jpe?g|png|webp)', src, re.I
    ):
        return ''
    host = urlsplit(src).hostname
    if host.endswith(('.local', '.localhost', '.internal', '.lan', '.test', '.invalid', '.example')):
        return ''
    for name in ('width', 'height'):
        size = re.fullmatch(r'([0-9]{1,5})(?:px)?', (attrs.get(name) or '').strip(), re.I)
        if size and int(size[1]) < 96:
            return ''
    return src


def body_text(part, images):
    headers = {item['name'].lower(): item['value'] for item in part.get('headers', [])}
    if part.get('filename') or 'attachment' in headers.get('content-disposition', '').lower():
        return []
    contents = [text for child in part.get('parts', []) for text in body_text(child, images)]
    mime = part.get('mimeType')
    data = part.get('body', {}).get('data')
    if mime not in {'text/plain', 'text/html'} or not data:
        return contents
    charset = re.search(r'charset\s*=\s*"?([\w.-]+)', headers.get('content-type', ''), re.I)
    raw = base64.urlsafe_b64decode(data + '=' * (-len(data) % 4))
    try:
        text = raw.decode(charset[1] if charset else 'utf-8', errors='replace')
    except LookupError:
        text = raw.decode('utf-8', errors='replace')
    if mime == 'text/html':
        parser = EmailHTML(images)
        parser.feed(text)
        text = ''.join(parser.text)
    return contents + [text]


def current_run(state, token):
    pending = state.get('pending')
    if not pending or pending['token'] != token:
        raise ValueError('This scan has been superseded or completed')
    return pending


def gmail_link(message_id, thread_id):
    """Link to an exact Message-ID search, falling back to the Gmail thread."""
    root = 'https://mail.google.com/mail/u/0/'
    message_id = message_id.strip().strip('<>')
    if re.fullmatch(r'[^\s<>]+@[^\s<>]+', message_id):
        return root + '#search/' + quote('rfc822msgid:' + message_id, safe='')
    if re.fullmatch(r'[0-9a-fA-F]+', thread_id):
        return root + '#all/' + thread_id
    return ''


def open_message(state, token, message_id):
    pending = current_run(state, token)
    received = pending['candidates'].get(message_id)
    if received is None:
        raise ValueError('Message was outside the selected scan window')
    result = {'id': message_id, 'expired': True}
    if fresh(received):
        with gmail() as session:
            message = request(session, '/' + message_id, format='full', fields='internalDate,threadId,payload')
        if fresh(int(message['internalDate'])):
            payload = message['payload']
            headers = {h['name'].lower(): h['value'] for h in payload.get('headers', [])}
            link = gmail_link(headers.get('message-id', ''), message.get('threadId', ''))
            if link:
                pending.setdefault('sources', {})[message_id] = link
            images = {}
            body = '\n\n'.join(body_text(payload, images))
            pending.setdefault('images', {})[message_id] = list(images)
            result = {
                'id': message_id,
                'received': dt.datetime.fromtimestamp(received / 1000, dt.timezone.utc).isoformat(),
                'subject': headers.get('subject', ''),
                'sender': headers.get('from', ''),
                'body': body,
                'images': [{'id': i, 'url': url, 'alt': alt}
                           for i, (url, alt) in enumerate(images.items(), 1)],
            }
    if message_id not in pending['opened']:
        pending['opened'].append(message_id)
    save(state)
    print(json.dumps(result, ensure_ascii=False))


def field(offer, name, default=''):
    value = offer.get(name, default)
    if not isinstance(value, str):
        raise ValueError(f'{name} must be text')
    return ' '.join(value.split())


def positive_amount(offer, name):
    amount = offer.get(name) or 0
    if type(amount) not in {float, int} or not math.isfinite(amount) or amount < 0:
        raise ValueError(f'{name} must be a non-negative number')
    return amount


def normalize_offer_text(text):
    return ' '.join(re.findall(r'[^\W_]+', unicodedata.normalize('NFKC', text).casefold()))


def delivery_record(offer, merchant, terms, percent, eur, deadline, code):
    return {
        'merchant': normalize_offer_text(merchant),
        'scope': normalize_offer_text(field(offer, 'offer_scope') or terms),
        'percent': float(percent),
        'eur': float(eur),
        'redemption': offer['redemption'],
        'code': unicodedata.normalize('NFKC', code).casefold(),
        'expires': deadline.astimezone(dt.timezone.utc).isoformat(),
    }


def delivery_key(record):
    # A reminder can extend a still-live offer without otherwise changing it.
    return json.dumps({key: value for key, value in record.items() if key != 'expires'},
                      sort_keys=True, ensure_ascii=False)


def markdown(text):
    """Keep email-derived text literal in Telegram's rich Markdown parser."""
    return re.sub(r'([\\`*_{}\[\]()#+.!|~$-])', r'\\\1', escape(text, quote=False))


def markdown_link(label, url):
    destination = quote(url, safe=':/?#[]@!$&\'*,;=+%~-._')
    return f'[{label}]({destination})'


def render_offers(offers, pending, state):
    groups = {}
    now = dt.datetime.now(dt.timezone.utc)
    seen = {delivery_key(record) for record in state.get('delivered_offers', [])
            if dt.datetime.fromisoformat(record['expires']) > now}
    legacy_sent = state.get('sent', {})
    for offer in offers:
        source = offer['id']
        if source not in pending['opened'] or not fresh(pending['candidates'][source]):
            raise ValueError('Offer did not come from an opened, recent message')
        merchant = field(offer, 'merchant')
        deadline = dt.datetime.fromisoformat(offer['expires'])
        if deadline.tzinfo is None:
            raise ValueError('Expiry must include its time zone')
        percent, eur = positive_amount(offer, 'percent'), positive_amount(offer, 'eur')
        if deadline <= now + dt.timedelta(hours=2) or (percent < 25 and eur < 10):
            continue
        redemption = offer['redemption']
        code = field(offer, 'code')
        if redemption not in {'code', 'credit', 'automatic'} or (redemption == 'code' and not code):
            raise ValueError('Offer needs a concrete redemption method')
        terms, expiry = field(offer, 'conditions'), field(offer, 'expiry_display')
        links = offer.get('links', [])
        if not merchant or not terms or not expiry or not isinstance(links, list) or not all(
            isinstance(link, str) and not any(char.isspace() for char in link) and valid_link(link)
            for link in links
        ):
            raise ValueError('Invalid offer summary fields')
        legacy_key = json.dumps([source, merchant.casefold(), deadline.isoformat(), code])
        record = delivery_record(offer, merchant, terms, percent, eur, deadline, code)
        key = delivery_key(record)
        if key in seen or legacy_key in legacy_sent:
            continue
        seen.add(key)
        discount = ' / '.join(part for part in [f'{percent:g}%' if percent else '', f'€{eur:g}' if eur else ''] if part)
        value = field(offer, 'price')
        excerpt = field(offer, 'english_excerpt')
        if not excerpt or len(excerpt) > 850:
            raise ValueError('Include a concise English excerpt of the source offer')
        actions = [markdown_link('Redeem offer' if i == 1 else f'Redeem option {i}', link)
                   for i, link in enumerate(links, 1)]
        gmail_url = pending.get('sources', {}).get(source)
        if gmail_url:
            actions.append(markdown_link('Open in Gmail', gmail_url))
        lines = [f'  - **Discount:** {discount}' + (f' ({markdown(value)})' if value else '')]
        if code:
            lines.append(f'  - **Code:** {markdown(code)}')
        lines += [f'  - **Expiry:** {markdown(expiry)}', f'  - **Conditions:** {markdown(terms)}']
        if actions:
            lines.append('  - ' + ' · '.join(actions))
        lines += ['', '  <details><summary>Offer details in English</summary>', '',
                  '  ' + markdown(excerpt), '']
        image_ids = offer.get('image_ids', [])
        if isinstance(image_ids, list):
            images = pending.get('images', {}).get(source, [])
            selected = list(dict.fromkeys(i for i in image_ids if type(i) is int and 1 <= i <= len(images)))
            for image_id in selected[:2]:
                lines += ['  !' + markdown_link(PHOTO_LABEL, images[image_id - 1]), '']
        lines.append('  </details>')
        block = '\n'.join(lines)
        _, blocks, records = groups.setdefault(record['merchant'], (merchant, [], []))
        blocks.append(block)
        records.append(record)
    for merchant, blocks, records in groups.values():
        yield f'- **{markdown(merchant)}**\n' + '\n\n'.join(blocks), records


def without_photos(text):
    return '\n'.join(line for line in text.split('\n') if not line.startswith(PHOTO_LINE))


def telegram_batch_fits(text):
    return (len(text) <= 32768 and text.count('\n' + PHOTO_LINE) <= 50
            and len(without_photos(text).encode('utf-16-le')) // 2 <= 3900)


def notify(text):
    # Use the same destination as notif-tlg-heart and send the rich source as data.
    command = ('[[ -n ${tlg_heart-} ]] || exit 1\n'
               'command tsend --parse-mode=rich -- "${tlg_heart}" "$1"')
    shell = next(path for path in ('/opt/homebrew/bin/zsh', '/usr/local/bin/zsh', '/bin/zsh')
                 if os.access(path, os.X_OK))
    args = [shell, '-lc', command, 'gmail-offers']
    result = subprocess.run(args + [text], capture_output=True, timeout=60)
    # A confirmed Telegram media rejection is safe to retry without photos.
    # A transport failure may have delivered the first request already.
    if result.returncode and re.search(
        rb'(?m)^tsend: Cannot send a rich message to .*?(?:WEBPAGE_CURL_FAILED|WEBPAGE_MEDIA_EMPTY|MEDIA_INVALID|'
        rb'PHOTO_INVALID|IMAGE_PROCESS_FAILED|RICH_MESSAGE_INVALID|CHAT_SEND_PHOTOS_FORBIDDEN|'
        rb'WebpageCurlFailedError|MediaInvalidError|PhotoInvalid\w*Error|ChatSendPhotosForbiddenError|'
        rb'failed to get HTTP URL content|wrong type of the web page content)', result.stderr, re.I
    ):
        plain = without_photos(text)
        if plain != text:
            result = subprocess.run(args + [plain], capture_output=True, timeout=60)
    if result.returncode:
        raise RuntimeError('Telegram delivery failed')


def finish_window(state, through):
    prune_history(state)
    state['checkpoint'] = through
    state.pop('pending', None)
    state.pop('sent', None)
    save(state)


def complete_scan(state, token, offers):
    pending = current_run(state, token)
    if set(pending['opened']) != set(pending['candidates']):
        raise ValueError('Open each selected message before completing the scan')
    if not isinstance(offers, list):
        raise ValueError('Expected a JSON array of offers')
    prune_history(state)
    batches = []
    text, delivered = '', []
    for block, records in render_offers(offers, pending, state):
        if not telegram_batch_fits(block):
            block = without_photos(block)
        if not telegram_batch_fits(block):
            raise ValueError('A merchant summary is too long for Telegram')
        joined = '\n'.join(filter(None, [text, block]))
        if not telegram_batch_fits(joined):
            batches.append((text, delivered))
            text, delivered = block, records
        else:
            text, delivered = joined, delivered + records
    if text:
        batches.append((text, delivered))
    for text, records in batches:
        notify(text)
        state.setdefault('delivered_offers', []).extend(records)
        save(state)
    finish_window(state, pending['through'])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest='command', required=True)
    sub.add_parser('fetch').add_argument('--probe', action='store_true')
    open_cmd = sub.add_parser('open')
    open_cmd.add_argument('token')
    open_cmd.add_argument('message_id')
    complete_cmd = sub.add_parser('email-complete-offer-scan', aliases=['finish'],
                                  help='Deliver qualifying offers and save the checkpoint.')
    complete_cmd.add_argument('token')
    complete_cmd.add_argument('--offers', type=Path)
    args = parser.parse_args()
    os.umask(0o077)
    STATE.mkdir(mode=0o700, exist_ok=True)
    with (STATE / 'lock').open('a') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        path = STATE / 'state.json'
        state = json.loads(path.read_text()) if path.exists() else {}
        if args.command == 'fetch':
            fetch(state, args.probe)
        elif args.command == 'open':
            open_message(state, args.token, args.message_id)
        else:
            complete_scan(state, args.token, json.loads(args.offers.read_text()) if args.offers else [])


if __name__ == '__main__':
    main()

#!/usr/bin/env python3
"""Read a bounded Gmail window and deliver qualified offers to Telegram."""

import argparse
import base64
import datetime as dt
import fcntl
from html.parser import HTMLParser
import json
import math
import os
from pathlib import Path
import re
import subprocess
import time
from urllib.parse import quote, urlsplit
from uuid import uuid4


ROOT = Path(os.environ.get('NIGHT_GMAIL_HOME', Path.home() / '.local/share/night-gmail'))
STATE = ROOT / 'offer-scan'
DAY = 86400
API = 'https://gmail.googleapis.com/gmail/v1/users/{}/messages'


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
                details = request(session, '/' + message_id, format='metadata', fields='internalDate')
                received = int(details['internalDate'])
                if start * 1000 <= received < now * 1000 and fresh(received):
                    candidates[message_id] = received
            if 'nextPageToken' not in page:
                break
            args['pageToken'] = page['nextPageToken']

    if probe:
        print(json.dumps({'candidates': len(candidates)}))
    elif candidates:
        pending = {'token': uuid4().hex, 'through': now, 'candidates': candidates, 'opened': []}
        state['pending'] = pending
        save(state)
        print(json.dumps({'token': pending['token'], 'ids': list(candidates)}))
    else:
        finish_window(state, now)


class EmailHTML(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.text = []
        self.hidden = None

    def handle_starttag(self, tag, attrs):
        if tag in {'style', 'script', 'template'}:
            self.hidden = tag
        if not self.hidden and tag in {'br', 'p', 'li', 'tr', 'div'}:
            self.text.append('\n')
        link = dict(attrs).get('href')
        if not self.hidden and tag == 'a' and link and valid_link(link):
            self.text.append(f' [Link: {link}] ')

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


def body_text(part):
    headers = {item['name'].lower(): item['value'] for item in part.get('headers', [])}
    if part.get('filename') or 'attachment' in headers.get('content-disposition', '').lower():
        return []
    contents = [text for child in part.get('parts', []) for text in body_text(child)]
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
        parser = EmailHTML()
        parser.feed(text)
        text = ''.join(parser.text)
    return contents + [text]


def current_run(state, token):
    pending = state.get('pending')
    if not pending or pending['token'] != token:
        raise ValueError('This scan has been superseded or completed')
    return pending


def open_message(state, token, message_id):
    pending = current_run(state, token)
    received = pending['candidates'].get(message_id)
    if received is None:
        raise ValueError('Message was outside the selected scan window')
    result = {'id': message_id, 'expired': True}
    if fresh(received):
        with gmail() as session:
            message = request(session, '/' + message_id, format='full', fields='internalDate,payload')
        if fresh(int(message['internalDate'])):
            payload = message['payload']
            headers = {h['name'].lower(): h['value'] for h in payload.get('headers', [])}
            result = {
                'id': message_id,
                'received': dt.datetime.fromtimestamp(received / 1000, dt.timezone.utc).isoformat(),
                'subject': headers.get('subject', ''),
                'sender': headers.get('from', ''),
                'body': '\n\n'.join(body_text(payload)),
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


def render_offers(offers, pending, sent):
    groups = {}
    now = dt.datetime.now(dt.timezone.utc)
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
        key = json.dumps([source, merchant.casefold(), deadline.isoformat(), code])
        if key in sent:
            continue
        discount = ' / '.join(part for part in [f'{percent:g}%' if percent else '', f'€{eur:g}' if eur else ''] if part)
        value = field(offer, 'price')
        block = (f'  - Discount: {discount}' + (f' ({value})' if value else '') + '\n'
                 f'  - Code: {code or "None required"}\n'
                 f'  - Expiry: {expiry}\n'
                 f'  - Conditions: {terms}\n'
                 f'  - Links: {", ".join(links) if links else "None stated"}')
        heading, blocks = groups.setdefault(merchant.casefold(), (merchant, {}))
        blocks[key] = block
    for merchant, blocks in groups.values():
        yield f'- {merchant}\n' + '\n'.join(blocks.values()), list(blocks)


def notify(text):
    # Treat the message as one argument. Disable the local fallback for this task.
    command = ('(( ${+functions[notif-tlg-heart]} )) && [[ -n ${tlg_heart-} ]] || exit 1\n'
               'function notif { return 1; }\n'
               'notif-tlg-heart "$1"')
    shell = next(path for path in ('/opt/homebrew/bin/zsh', '/usr/local/bin/zsh', '/bin/zsh')
                 if os.access(path, os.X_OK))
    result = subprocess.run([shell, '-lc', command, 'gmail-offers', text], capture_output=True, timeout=60)
    if result.returncode:
        raise RuntimeError('Telegram delivery failed')


def finish_window(state, through):
    state['checkpoint'] = through
    state.pop('pending', None)
    state.pop('sent', None)
    save(state)


def finish(state, token, offers):
    pending = current_run(state, token)
    if set(pending['opened']) != set(pending['candidates']):
        raise ValueError('Open each selected message before completing the scan')
    if not isinstance(offers, list):
        raise ValueError('Expected a JSON array of offers')
    sent = state.setdefault('sent', {})
    for key, stamp in list(sent.items()):
        if not fresh(stamp):
            del sent[key]
    batches = []
    text, keys = '', []
    for block, block_keys in render_offers(offers, pending, sent):
        if len(block.encode('utf-16-le')) // 2 > 3900:
            raise ValueError('A merchant summary is too long for Telegram')
        joined = '\n'.join(filter(None, [text, block]))
        if len(joined.encode('utf-16-le')) // 2 > 3900:
            batches.append((text, keys))
            text, keys = block, block_keys
        else:
            text, keys = joined, keys + block_keys
    if text:
        batches.append((text, keys))
    for text, keys in batches:
        notify(text)
        sent.update({key: pending['candidates'][json.loads(key)[0]] for key in keys})
        save(state)
    finish_window(state, pending['through'])


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    sub = parser.add_subparsers(dest='command', required=True)
    sub.add_parser('fetch').add_argument('--probe', action='store_true')
    open_cmd = sub.add_parser('open')
    open_cmd.add_argument('token')
    open_cmd.add_argument('message_id')
    finish_cmd = sub.add_parser('finish')
    finish_cmd.add_argument('token')
    finish_cmd.add_argument('--offers', type=Path)
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
            finish(state, args.token, json.loads(args.offers.read_text()) if args.offers else [])


if __name__ == '__main__':
    main()

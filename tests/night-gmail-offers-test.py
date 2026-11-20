#!/usr/bin/env python3
"""Exercise Gmail offer history without reading mail or sending notifications."""

from contextlib import redirect_stdout
import datetime as dt
import importlib.util
import io
import json
from pathlib import Path
import tempfile
import time
import unittest
from unittest.mock import patch


ROOT = Path(__file__).resolve().parents[1]
SPEC = importlib.util.spec_from_file_location('gmail_offers', ROOT / 'email/bin/night-gmail-offers.py')
offers = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(offers)


class OfferHistoryTest(unittest.TestCase):
    def setUp(self):
        scratch = self.enterContext(tempfile.TemporaryDirectory())
        self.directory = Path(scratch)
        self.enterContext(patch.object(offers, 'STATE', self.directory))
        self.notify = self.enterContext(patch.object(offers, 'notify'))
        self.now = dt.datetime.now(dt.timezone.utc)
        self.through = int(time.time())

    def offer(self, message_id='one', **changes):
        offer = {
            'id': message_id, 'merchant': 'Example Store', 'offer_scope': 'Wool cardigans',
            'percent': 30, 'code': 'WOOL30', 'expires': (self.now + dt.timedelta(days=2)).isoformat(),
            'expiry_display': 'Friday', 'redemption': 'code', 'conditions': 'Use the code at checkout.',
            'english_excerpt': 'All wool cardigans are 30% off with this code.',
            'links': ['https://example.org/wool?ref=original'],
        }
        return offer | changes

    def start(self, state, *ids):
        state['pending'] = {'token': 'scan', 'through': self.through,
                            'candidates': dict.fromkeys(ids, self.through * 1000), 'opened': list(ids)}
        return state

    def saved(self):
        return json.loads((self.directory / 'state.json').read_text())

    def test_reminders_and_material_changes_across_runs(self):
        original = self.offer()
        reminder = self.offer('two', merchant=' EXAMPLE  store ', offer_scope='Wool-cardigans',
                              percent=30.0, code='wool30', links=['https://example.org/wool?ref=reminder'],
                              expires=(self.now + dt.timedelta(days=3)).isoformat())
        offers.complete_scan(self.start({}, 'one', 'two'), 'scan', [original, reminder])
        state = self.saved()
        self.assertEqual(len(state['delivered_offers']), 1)

        stronger = self.offer('two', percent=40)
        different_scope = self.offer('two', offer_scope='Wool coats')
        offers.complete_scan(self.start(state, 'two'), 'scan', [reminder, stronger, different_scope])
        self.assertEqual(self.notify.call_count, 2)
        second_message = self.notify.call_args.args[0]
        self.assertEqual(second_message.count('**Discount:**'), 2)
        self.assertNotIn('ref=reminder', second_message)
        self.assertEqual(len(self.saved()['delivered_offers']), 3)

        offers.complete_scan(self.start(self.saved(), 'two'), 'scan', [reminder])
        self.assertEqual(self.notify.call_count, 2)

    def test_retry_skips_batches_already_delivered(self):
        submitted = [self.offer(key, merchant='Merchant ' + key,
                                conditions='Eligibility ' * 65, english_excerpt='Terms ' * 130)
                     for key in ('one', 'two', 'three')]
        state = self.start({'checkpoint': self.through - 10}, 'one', 'two', 'three')
        self.notify.side_effect = [None, RuntimeError('delivery refused')]
        with self.assertRaisesRegex(RuntimeError, 'delivery refused'):
            offers.complete_scan(state, 'scan', submitted)
        saved = self.saved()
        self.assertEqual(saved['checkpoint'], self.through - 10)
        self.assertEqual(len(saved['delivered_offers']), 2)

        self.notify.reset_mock(side_effect=True)
        offers.complete_scan(saved, 'scan', submitted)
        self.notify.assert_called_once()
        self.assertIn('Merchant three', self.notify.call_args.args[0])
        self.assertNotIn('Merchant one', self.notify.call_args.args[0])
        self.assertEqual(self.saved()['checkpoint'], self.through)

    def test_quiet_fetch_prunes_old_records_and_skips_direct_keepa(self):
        stale = {'expires': (self.now - dt.timedelta(days=8)).isoformat()}
        grace = {'expires': (self.now - dt.timedelta(days=6)).isoformat()}
        active = {'expires': (self.now + dt.timedelta(days=30)).isoformat()}
        state = {'delivered_offers': [stale, grace, active]}
        (self.directory / 'query').write_text('category:promotions')
        meta = {'internalDate': (self.through - 2) * 1000,
                'payload': {'headers': [{'name': 'From', 'value': 'Keepa <alert@notify.KEEPA.com>'}]}}
        with patch.object(offers, 'gmail'), patch.object(offers, 'request', side_effect=[
            {'messages': [{'id': 'keepa'}]}, meta,
        ]) as request, redirect_stdout(io.StringIO()) as output:
            offers.fetch(state)
        self.assertEqual(request.call_args.kwargs['metadataHeaders'], 'From')
        self.assertEqual(output.getvalue(), '')
        self.assertEqual(self.saved()['delivered_offers'], [grace, active])
        self.assertNotIn('pending', self.saved())

    def test_expired_campaign_can_renew_during_grace_period(self):
        old_offer = self.offer(expires=(self.now - dt.timedelta(days=1)).isoformat())
        old = offers.delivery_record(old_offer, old_offer['merchant'], old_offer['conditions'],
                                     old_offer['percent'], 0, dt.datetime.fromisoformat(old_offer['expires']),
                                     old_offer['code'])
        state = self.start({'delivered_offers': [old]}, 'one')
        offers.complete_scan(state, 'scan', [self.offer()])
        self.notify.assert_called_once()
        self.assertEqual(len(self.saved()['delivered_offers']), 2)


if __name__ == '__main__':
    unittest.main()

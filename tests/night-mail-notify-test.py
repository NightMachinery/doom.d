#!/usr/bin/env python3
"""Hermetic mail-policy and real notmuch-hook tests. No network or real alerts."""
import importlib.machinery
import importlib.util
import json
import os
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader("mail_notify", str(ROOT / "email/bin/night-mail-notify"))
spec = importlib.util.spec_from_loader(loader.name, loader)
notify = importlib.util.module_from_spec(spec)
loader.exec_module(notify)


class PolicyTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="mail-policy-", dir="/private/tmp")
        self.addCleanup(self.tmp.cleanup)
        self.filename = Path(self.tmp.name) / "notifications.json"

    def config(self, groups, defaults=None):
        self.filename.write_text(json.dumps({"defaults": defaults or {}, "groups": groups}))
        return notify.load_config(self.filename)

    def test_order_and_combined_case_insensitive_matches(self):
        groups = self.config([
            {"name": "quiet", "match": {"from": "robot@", "subject": "digest"}, "enabled": False},
            {"name": "urgent", "match": {"subject": "urgent"}, "bypass_dnd": True},
            {"name": "rest"}], {"sound": "Glass"})
        messages = [{"from": "ROBOT@example.org", "subject": "Urgent DIGEST"},
                    {"from": "human@example.org", "subject": "URGENT"},
                    {"from": "robot@example.org", "subject": "Normal"}]
        items = list(notify.notifications(messages, groups))
        self.assertEqual(len(items), 2)
        self.assertIn("urgent", items[0][0])
        self.assertTrue(items[0][3]["bypass_dnd"])
        self.assertFalse(items[1][3]["bypass_dnd"])
        self.assertEqual(items[0][3]["sound"], "Glass")
        self.assertIn("rest", items[1][0])

    def test_validation(self):
        for groups in ([], [{"name": "a", "match": {"from": "x"}}],
                       [{"name": "a"}, {"name": "b"}],
                       [{"name": "a", "match": {"from": "["}}, {"name": "b"}],
                       [{"name": "a", "match": {"body": "x"}}, {"name": "b"}],
                       [{"name": "a", "match": {"from": "x"}}, {"name": "a"}],
                       [{"name": "a", "bypass_dnd": "false"}],
                       [{"name": "a", "max_individual": True}],
                       [{"name": "a", "typo": True}]):
            with self.subTest(groups=groups), self.assertRaises(ValueError):
                self.config(groups)
        self.filename.write_text('{"groups": [')
        with self.assertRaisesRegex(ValueError, "invalid JSON at line"):
            notify.load_config(self.filename)

    def test_defaults_missing_headers_and_bursts(self):
        groups = notify.load_config(self.filename)
        self.assertFalse(groups[0]["bypass_dnd"])
        messages = [{"from": "", "subject": ""}] * 4
        items = list(notify.notifications(messages[:3], groups))
        self.assertEqual(len(items), 3)
        self.assertEqual(items[0][1:3], ("Unknown sender", "(No subject)"))
        items = list(notify.notifications(messages, groups))
        self.assertEqual(len(items), 1)
        self.assertIn("4 new unread messages", items[0][2])
        groups[0]["show_details"] = False
        self.assertEqual(list(notify.notifications(messages[:1], groups))[0][2], "1 new unread message")

    def test_groups_batch_independently(self):
        groups = self.config([{"name": "a", "match": {"from": "a"}, "max_individual": 1}, {"name": "b"}])
        messages = [{"from": "a", "subject": "A"}] * 2 + [{"from": "b", "subject": "B"}]
        self.assertEqual(len(list(notify.notifications(messages, groups))), 2)

    def test_decoding_literal_arguments_and_dnd(self):
        self.assertEqual(notify.decode("=?utf-8?b?0J/RgNC40LLQtdGC?="), "Привет")
        group = dict(notify.DEFAULTS)
        calls = []
        with patch.object(notify, "run", side_effect=lambda args, **kwargs: calls.append(args)):
            notify.deliver([("[title]", "-sender", '$(printf inert) "quoted"', group)])
            group = dict(group, bypass_dnd=True, sound="Glass")
            notify.deliver([("title", "sender", "subject", group)])
        self.assertNotIn("-ignoreDnD", calls[0])
        self.assertIn("-ignoreDnD", calls[1])
        self.assertIn('\\$(printf inert) "quoted"', calls[0])
        self.assertEqual(notify.clean("a\n\x1bb\u202ec"), "a b c")

    def test_config_creation_preserves_user_file(self):
        env = dict(os.environ, NIGHT_MAIL_NOTIFY_CONFIG=str(self.filename))
        command = ["python3", str(ROOT / "email/bin/night-mail-notify"), "--init-config"]
        subprocess.run(command, env=env, check=True, stdout=subprocess.DEVNULL)
        self.assertEqual(self.filename.stat().st_mode & 0o777, 0o600)
        self.filename.write_text('{"groups":[{"name":"My policy","enabled":false}]}')
        before = self.filename.read_bytes()
        subprocess.run(command, env=env, check=True, stdout=subprocess.DEVNULL)
        self.assertEqual(self.filename.read_bytes(), before)


@unittest.skipUnless(shutil.which("notmuch"), "notmuch is required for hook integration")
class HookTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="mail-hooks-", dir="/private/tmp")
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        self.home = self.base / "home"
        self.root = self.home / "Mail"
        for folder in ("INBOX", "Sent", "Trash", "Drafts"):
            for part in ("new", "cur", "tmp"):
                (self.root / "work" / folder / part).mkdir(parents=True)
        self.hooks = self.base / "email/notmuch-hooks"
        self.hooks.mkdir(parents=True)
        (self.base / "email/bin").mkdir()
        for name in ("bin/night-mail-lib.bash", "bin/night-mail-notify", "notmuch-hooks/pre-new", "notmuch-hooks/post-new"):
            shutil.copy2(ROOT / "email" / name, self.base / "email" / name)
        # Bash imports this inert mbsync function for the real pre-new hook.
        # The test must never contact an IMAP server.
        bash_env = self.base / "bash-env"
        bash_env.write_text("mbsync() { return 0; }\n")
        self.config = self.base / "notmuch-config"
        self.config.write_text(f'''[database]
path={self.root}
hook_dir={self.hooks}
[user]
name=Fixture
primary_email=fixture@example.org
[new]
tags=new
[maildir]
synchronize_flags=true
[search]
exclude_tags=spam;deleted
''')
        self.alerts = self.base / "alerts.jsonl"
        helper = self.home / "Applications/Night Mail.app/Contents/MacOS/terminal-notifier"
        helper.parent.mkdir(parents=True)
        helper.write_text('''#!/usr/bin/env python3
import json,os,sys
with open(os.environ["TEST_ALERTS"],"a") as stream: stream.write(json.dumps(sys.argv[1:])+"\\n")
sys.exit(int(os.environ.get("TEST_NOTIFY_EXIT","0")))
''')
        helper.chmod(0o755)
        self.revision = self.home / ".local/state/night-mail/lastmod-before-sync"
        self.revision.parent.mkdir(parents=True)
        self.revision.write_text("0\n")
        self.initial = self.revision.with_name("initial-import")
        self.initial.write_text("1\n")
        self.policy = self.base / "notifications.json"
        self.env = dict(os.environ, HOME=str(self.home), NOTMUCH_CONFIG=str(self.config),
                        TEST_ALERTS=str(self.alerts), NIGHT_MAIL_NOTIFY_CONFIG=str(self.policy),
                        BASH_ENV=str(bash_env))

    def nm(self, *args):
        result = subprocess.run(["notmuch", *args], env=self.env, timeout=30,
                                capture_output=True, text=True)
        self.assertEqual(result.returncode, 0, result.stderr)
        return result

    def mail(self, name, folder="INBOX", flags="", subject="Fixture subject", sender="fixture@example.org"):
        mail = self.root / "work" / folder / "new" / (name + ":2," + flags)
        mail.write_text(f"From: {sender}\nTo: fixture@example.org\nSubject: {subject}\n"
                        f"Message-ID: <{name}@fixture.invalid>\nDate: Fri, 16 Oct 2026 10:00:00 +0000\n\nBody.\n")
        return mail

    def sync(self):
        result = self.nm("new", "--quiet")
        revision = self.nm("count", "--lastmod", "*").stdout.strip().split("\t")[2]
        self.revision.write_text(revision)
        self.initial.write_text("0\n")
        return result

    def delivered(self):
        return [json.loads(line) for line in self.alerts.read_text().splitlines()] if self.alerts.exists() else []

    def test_first_import_new_arrival_repeat_flags_and_duplicates(self):
        self.mail("existing")
        self.sync()
        self.assertFalse(self.delivered())
        self.mail("arrival", subject="[Important] $(printf inert)")
        self.sync()
        self.assertEqual(len(self.delivered()), 1)
        self.assertNotIn("-ignoreDnD", self.delivered()[0])
        self.assertEqual(self.nm("count", "tag:new").stdout.strip(), "0")
        self.sync()
        self.nm("tag", "+flagged", "--", "*")
        self.sync()
        arrival = next(self.root.glob("work/INBOX/*/arrival:2,*"))
        shutil.copy2(arrival, self.root / "work/INBOX/new/duplicate:2,")
        self.sync()
        self.assertEqual(len(self.delivered()), 1)

    def test_only_eligible_inbox_arrivals(self):
        self.sync()
        self.mail("read", flags="S")
        self.mail("sent", folder="Sent")
        self.mail("draft", folder="Drafts", flags="D")
        self.mail("trash", folder="Trash")
        self.mail("inbox")
        self.sync()
        self.assertEqual(len(self.delivered()), 1)
        self.assertIn("\\Fixture subject", self.delivered()[0])

    def test_failure_does_not_block_tag_cleanup_or_backups(self):
        self.sync()
        self.mail("failure")
        self.env["TEST_NOTIFY_EXIT"] = "1"
        self.sync()
        self.assertEqual(self.nm("count", "tag:new").stdout.strip(), "0")
        self.assertEqual(self.nm("count", "tag:inbox").stdout.strip(), "1")
        self.assertTrue(list((self.home / ".local/share/night-mail/tag-dumps").glob("*.gz")))
        self.sync()
        self.assertEqual(len(self.delivered()), 1)

    def test_invalid_and_disabled_policy_no_backlog(self):
        self.sync()
        self.policy.write_text("invalid")
        self.mail("invalid-policy")
        self.sync()
        self.assertFalse(self.delivered())
        self.policy.write_text('{"groups":[{"name":"rest","enabled":false}]}')
        self.mail("disabled")
        self.sync()
        self.policy.unlink()
        self.sync()
        self.assertFalse(self.delivered())

    def test_database_rebuild_does_not_realert_old_mail(self):
        self.mail("original")
        self.sync()
        shutil.rmtree(self.root / ".notmuch")  # This test's synthetic database only.
        self.sync()
        self.assertFalse(self.delivered())
        self.mail("after-rebuild")
        self.sync()
        self.assertEqual(len(self.delivered()), 1)

    def test_multiple_accounts_share_one_notification_app(self):
        for part in ("new", "cur", "tmp"):
            (self.root / "second/INBOX" / part).mkdir(parents=True)
        self.sync()
        self.mail("first-account")
        mail = self.mail("second-account")
        mail.rename(self.root / "second/INBOX/new/second-account:2,")
        self.sync()
        self.assertEqual(len(self.delivered()), 2)
        self.assertEqual(self.nm("count", "tag:inbox").stdout.strip(), "2")


if __name__ == "__main__":
    unittest.main()

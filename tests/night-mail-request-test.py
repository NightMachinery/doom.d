#!/usr/bin/env python3
"""Request coordination uses fake launchctl; never starts a real mail worker."""
import importlib.machinery
import importlib.util
import os
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
loader = importlib.machinery.SourceFileLoader("mail_request", str(ROOT / "email/bin/night-mail-request"))
spec = importlib.util.spec_from_loader(loader.name, loader)
requester = importlib.util.module_from_spec(spec)
loader.exec_module(requester)


class RequestTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory(prefix="mail-request-", dir="/private/tmp")
        self.addCleanup(self.tmp.cleanup)
        self.home = Path(self.tmp.name)
        self.state = self.home / ".local/state/night-mail"
        env = patch.dict(os.environ, HOME=str(self.home))
        env.start()
        self.addCleanup(env.stop)

    def test_fresh_result_status_is_returned_and_only_new_log_is_shown(self):
        log = self.home / "logs/night-mail-sync.log"
        log.parent.mkdir()
        log.write_text("old failure\n")
        calls = []

        def launchctl(*args):
            calls.append(args)
            if args[0] == "kickstart":
                token = (self.state / "launchd-request").read_text().strip()
                requester.atomic_text(self.state / "launchd-result", token + "\n69\n")
                with log.open("a") as stream:
                    stream.write("current fetch failed\n")
            return subprocess.CompletedProcess(args, 0, "arguments = { --launchd }\nstate = not running\n", "")

        with patch.object(requester, "launchctl", side_effect=launchctl), patch.object(requester.time, "sleep"), patch.object(requester.sys.stdout, "write") as output:
            self.assertEqual(requester.request(5), 69)
        self.assertIn(("kickstart", f"gui/{os.getuid()}/{requester.LABEL}"), calls)
        output.assert_called_once_with("current fetch failed\n")

    def test_existing_running_sync_does_not_satisfy_a_new_request(self):
        calls = []
        clock = [0.0]

        def launchctl(*args):
            calls.append(args)
            token_file = self.state / "launchd-request"
            if token_file.exists() and args[0] == "print":
                requester.atomic_text(self.state / "launchd-result", "older-request\n0\n")
            if args[0] == "kickstart":
                token = token_file.read_text().strip()
                requester.atomic_text(self.state / "launchd-result", token + "\n0\n")
            running = sum(call[0] == "print" for call in calls) <= 2
            return subprocess.CompletedProcess(args, 0, "arguments = { --launchd }\n" + ("pid = 123\n" if running else ""), "")

        with patch.object(requester, "launchctl", side_effect=launchctl), \
             patch.object(requester.time, "monotonic", side_effect=lambda: clock[0]), \
             patch.object(requester.time, "sleep", side_effect=lambda _: clock.__setitem__(0, clock[0] + 0.5)):
            self.assertEqual(requester.request(5), 0)
        self.assertEqual(sum(args[0] == "kickstart" for args in calls), 1)
        self.assertGreaterEqual(sum(args[0] == "print" for args in calls), 3)

    def test_missing_or_outdated_agent_is_actionable(self):
        for output in ("", "arguments = { /bin/bash night-mail-sync }"):
            with patch.object(requester, "launchctl", return_value=subprocess.CompletedProcess([], 0, output, "")):
                with self.assertRaisesRegex(RuntimeError, "install-night-mail"):
                    requester.request(1)

    def test_periodic_tick_racing_kickstart_is_not_a_failure(self):
        kicked = [False]

        def launchctl(*args):
            if args[0] == "kickstart":
                kicked[0] = True
                token = (self.state / "launchd-request").read_text().strip()
                requester.atomic_text(self.state / "launchd-result", token + "\n0\n")
                return subprocess.CompletedProcess(args, 37, "", "already running")
            output = "arguments = { --launchd }\n" + ("pid = 123\n" if kicked[0] else "")
            return subprocess.CompletedProcess(args, 0, output, "")

        with patch.object(requester, "launchctl", side_effect=launchctl), patch.object(requester.time, "sleep"):
            self.assertEqual(requester.request(5), 0)

    def test_timeout_does_not_stop_or_restart_the_worker(self):
        calls = []

        def launchctl(*args):
            calls.append(args)
            return subprocess.CompletedProcess(args, 0, "arguments = { --launchd }\npid = 123\n", "")

        with patch.object(requester, "launchctl", side_effect=launchctl):
            with self.assertRaisesRegex(TimeoutError, "may still be running"):
                requester.request(0.2)
        self.assertTrue(all(args[0] == "print" for args in calls))


if __name__ == "__main__":
    unittest.main()

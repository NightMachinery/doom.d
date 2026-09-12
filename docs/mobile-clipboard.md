# Mobile terminal clipboard

`emc-mobile` already marks its terminal frame with `night/mobile=t`. Copies
and kills from that frame now use Emacs's built-in OSC 52 sender. The escape
sequence travels through the terminal connection to the client clipboard.
No listener, network address, credentials, or phone background service is needed.

Routing is decided at copy time from the selected frame, so a shared buffer
can be copied from mobile and desktop frames without changing its clipboard
settings. Unmarked frames retain their existing clipboard route. Mobile copies
do not also invoke the daemon host's clipboard command or legacy TCP sender.
Existing Org copy transformations and whitespace filtering still apply.

The implementation temporarily enables the `term/xterm.el` clipboard-write
backend for one operation and calls `gui-set-selection` for `CLIPBOARD`.
It restores the terminal capability even after errors. It never enables OSC 52
clipboard reads. Plain `emc-mobile` keeps its existing paste behavior; use the
terminal's Paste action to insert the client clipboard. The SSH variants below
also support programmatic clipboard reads.

## SSH fallback variants

`emc-tealy` and `emc-tealy-tmux` additionally mark the frame with
`night/clipboard-ssh-host="tealy"`. Text above the OSC 52 limit is passed to
`termux-clipboard-set` over SSH, using UTF-8 stdin. The destination is resolved
through the Emacs host's existing SSH configuration; no address or credentials
are stored in this repository. The tmux variant uses a separate `emacs-tealy`
session so it cannot accidentally attach to a plain mobile frame.

These SSH-marked frames also fetch fresh clipboard text with
`termux-clipboard-get` for normal yanks and paste commands using `current-kill`.
Routing follows the selected frame, including when the same buffer is visible
on desktop and mobile. Clipboard contents are not cached, and kill-ring
rotation remains local. Each read adds an SSH round trip; Termux's Paste action
is still useful when latency matters.

Automatic reads require both the SSH-marked mobile frame and
`night/ssh-paste-enabled-p`, which defaults to `t`. Use
`M-x night/ssh-paste-toggle` to toggle automatic reads across this daemon without
changing frame markers or copy routing. Set the variable to `nil` in your
configuration for a persistent opt-out. While disabled, ordinary paste uses
its previous provider.

Explicit commands bypass that toggle: `M-x night/ssh-paste` inserts text from
the selected frame's configured `night/clipboard-ssh-host`, and
`M-x night/tealy-paste` reads the `tealy` alias regardless of the frame.
Frame-based reads use the optional `night/clipboard-ssh-paste-command` parameter
or `termux-clipboard-get` by default. The override is a trusted remote shell
command, not clipboard data. A missing host for `night/ssh-paste` is an error;
it never silently chooses a different host.

The read waits for any queued copy to finish and shares a single overall
`night/mobile-clipboard-ssh-timeout` deadline (15 seconds by default). `C-g`
cancels the wait. SSH/read failures raise an error rather than pasting stale
text or the daemon host's clipboard. Reads do not use or update the copy
readiness cache: Android can restrict clipboard reads independently of writes,
especially while Termux is in the background. The Termux:API clipboard getter,
SSH keys, and trusted host key must be configured on the relevant hosts.

The first oversized copy checks authenticated SSH access and availability of
the clipboard command. Successful checks and transfers cache readiness for
900 seconds (`night/mobile-clipboard-ssh-cache-ttl`). Failures immediately replace
that state with a 30-second negative cache (`night/mobile-clipboard-ssh-failure-ttl`).
The shorter failure TTL lets a phone recover promptly after a temporary outage.
TTL expiration is lazy: there is no periodic polling or network work on launch.
Small copies need no SSH check. A cached success never guarantees the phone is
still reachable; every transfer's exit status is checked.

`M-x night/mobile-clipboard-cache-clear` clears all readiness entries in the
current Emacs daemon. `M-x night/mobile-clipboard-tealy-cache-clear` clears only
the `tealy` entry, preserving other hosts' cached readiness.
The shell command `emc-tealy-cache-clear` clears the same entry, honoring
the mobile launcher's dedicated-daemon setting. From Lisp, pass a host alias
to `night/mobile-clipboard-cache-clear` to clear just that entry. The cache is
memory-only and separate for each daemon. Clearing does not cancel in-flight
work, whose completion may populate the cache again.

SSH is non-interactive: keys and host trust must already be configured. It uses
a three-second connection timeout and a 15-second overall operation deadline
(`night/mobile-clipboard-ssh-timeout`). Text and remote output are not logged.
Copies run asynchronously, one per host. Only the newest waiting copy is
kept; a later small copy waits for an active SSH operation before using OSC 52,
preserving copy order during normal completion. Failed copies stay in the kill
ring and are never automatically replayed when the phone reconnects. As with
other remote writes, a lost reply or timeout can leave the remote result unknown.

SSH avoids the terminal parser's size limit, but Android and receiving apps
still impose their own clipboard constraints. A successful command exit is not
a guarantee that every destination app will accept a large paste. This fallback
is triggered by size only; it cannot detect silent OSC 52 rejection by tmux or
the terminal, so the tmux configuration below is still required for small copies.

## Existing support

Emacs ships OSC 52 support in `term/xterm.el` (available since Emacs 25).
The installed Emacs 29 implementation encodes UTF-8 and base64 and writes to
the selected terminal. Native support was not enabled on the inspected mobile
frame before this change.

Clipetty is also declared and required by this configuration, but its global
mode was commented out and its buffer mode was disabled. Enabling its ordinary
mode is buffer-local, affects every terminal view of that buffer, and chains
to the original clipboard function. The native backend with frame routing fits
the shared-daemon workflow without needing a new package or a second copy.

## Limits and tmux

Termux supports OSC 52 clipboard writes. The compatibility target, Termux
0.118.1, has an 8192-character OSC buffer; base64 reduces the text capacity to about
6 KiB. `night/mobile-clipboard-max-bytes` defaults to 6000 UTF-8 bytes, including
newlines. Larger copies stay in the kill ring and produce an explanatory message,
without sending a partial sequence. Increase the limit only for terminals and
multiplexers known to support larger messages (newer Termux code raises this
limit). OSC 52 has no success reply, so
a completed send cannot prove the receiving terminal accepted it.

For `emc-mobile-tmux`, the tmux server carrying Emacs must allow application
clipboard sequences. Opt in with `tmux set-option -s set-clipboard on` and verify the
attached terminal has the `Ms` capability (`tmux info`). Persist that option
in your tmux config if desired. No global tmux settings are changed by this
feature. Every tmux layer needs suitable configuration; a detached session has
no terminal clipboard to target. Any application in a pane can set the client
clipboard when `set-clipboard on` is enabled. Direct `emc-mobile` avoids this
extra configuration.

Regression checks: `emacs --batch -Q -l tests/night-mobile-clipboard-test.el`.
Tests capture native OSC 52 output rather than touching a real clipboard and
cover Unicode, newlines, frame isolation, append behavior, byte limits, errors,
existing whitespace policy, positive/negative cache expiration and clearing,
ordered copies, real UTF-8 pipe delivery, and process deadlines. SSH process
tests use local stand-ins and do not overwrite a device clipboard.

## Sources

- [Emacs 29 native OSC 52 implementation](https://github.com/emacs-mirror/emacs/blob/emacs-29/lisp/term/xterm.el)
- [Clipetty](https://github.com/spudlyo/clipetty)
- [Termux 0.118.1 terminal implementation](https://github.com/termux/termux-app/blob/v0.118.1/terminal-emulator/src/main/java/com/termux/terminal/TerminalEmulator.java)
- [tmux clipboard configuration](https://github.com/tmux/tmux/wiki/Clipboard)

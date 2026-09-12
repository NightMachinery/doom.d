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
clipboard reads. Paste behavior is unchanged; use the terminal's Paste action
to insert the client clipboard.

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
and existing whitespace policy.

## Sources

- [Emacs 29 native OSC 52 implementation](https://github.com/emacs-mirror/emacs/blob/emacs-29/lisp/term/xterm.el)
- [Clipetty](https://github.com/spudlyo/clipetty)
- [Termux 0.118.1 terminal implementation](https://github.com/termux/termux-app/blob/v0.118.1/terminal-emulator/src/main/java/com/termux/terminal/TerminalEmulator.java)
- [tmux clipboard configuration](https://github.com/tmux/tmux/wiki/Clipboard)

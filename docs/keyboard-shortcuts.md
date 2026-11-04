# Keyboard shortcuts

`Option+Shift+2` inserts `€` at point in Evil insert and normal states, keeping
the current state. Run `M-x night/insert-euro` to invoke the same command.

The binding is `M-@`, the event produced with Option mapped to Meta on a US
macOS keyboard. It replaces `mark-word` in those two states. Other keyboard
layouts or terminal modifier settings can produce a different event; use
`C-h k` to inspect the key if it does not work.

The command and binding live in `autoload/night-shortcuts-hotkeybindings.el`.

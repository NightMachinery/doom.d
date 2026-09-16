# Elisp Guidelines

Read `$NIGHTDIR/PE/elisp.org`.

# `emc-eval`
You can evaluate code in a running emacs server using:

```zsh
emc-eval '(print "Hello")'
```

`emc-eval` is a Zsh function, not a command.

When checking results from `emc-eval`, prefer returning values or printing with
`prin1`. Do not rely on `message` output as the command result.

`emc-eval` evaluates inside the running Emacs server's `default-directory`, not
necessarily the shell command's current directory. Use absolute paths for
`load-file` checks and reloads.

Avoid returning unreadable objects such as functions, buffers, windows, or
markers from `emc-eval`; coerce checks to booleans, strings, numbers, symbols,
or simple lists.

## Emacs Auto-Reload

After finishing development, use `emc-eval` AND `withemcgui emc-eval` to re-load all changed Elisp files so that the running servers always run up-to-date code.

### A reload is not a clean load

Two things it will not do, both of which present as "my new code does not
work" when the code on disk is fine:

- `defvar` and `defcustom` do **not** re-evaluate when the symbol is already
  bound, so an edited default or table keeps its old value. `makunbound` it
  before reloading.
- `after!` bodies accumulate in `after-load-alist` — each load appends rather
  than replaces — so a later `provide` replays *every* historical body, and a
  definition you just deleted or renamed can come back.

So reload freely to exercise behaviour, but it is never evidence that something
is gone or updated. Confirm renames, deletions and `defvar` edits in
`emacs -Q --batch`, stubbing the few load-time macros the file needs.

# Yasnippet Guidelines

Yasnippet includes everything after the `# --` marker in the snippet body, so a
trailing newline in the file becomes part of every expansion. Be deliberate
about it: end the file without a newline for inline snippets (e.g. timestamps),
with one when the expansion should end with a newline.

# Commit Guidelines

Never commit `./data/ispell-personal-dictionary`, as it might leak personal info. Tell the user to commit it themselves if need be.

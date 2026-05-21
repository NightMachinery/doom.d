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

# Commit Guidelines

Never commit `./data/ispell-personal-dictionary`, as it might leak personal info. Tell the user to commit it themselves if need be.

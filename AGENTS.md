# Elisp Guidelines

Read `$NIGHTDIR/PE/elisp.org`.

# `emc-eval`
You can evaluate code in a running emacs server using:

```zsh
emc-eval '(print "Hello")'
```

`emc-eval` is a Zsh function, not a command.

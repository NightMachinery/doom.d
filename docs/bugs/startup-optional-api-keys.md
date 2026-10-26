# Missing optional API keys during startup

A fresh machine can have a working shell bridge with no optional API credential.
The bridge may return an empty string or signal `end-of-file`; either should
leave the corresponding key unset, so Doom and unrelated tools still start.
`night/h-api-key-get` handles those two outcomes and leaves other bridge errors
visible. It never logs credential values.

Run the isolated regression test without loading personal shell configuration:

```sh
emacs -Q --batch -l tests/night-api-key-test.el -f ert-run-tests-batch-and-exit
```

After installing the configuration, verify a real daemon and graphical frame.
Wait for startup's server restart before evaluating a proof expression through
emacsclient; an early socket can disappear while initialization is still running.

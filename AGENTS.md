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

Three things it will not do, all of which present as "my new code does not
work" when the code on disk is fine:

- `defvar` and `defcustom` do **not** re-evaluate when the symbol is already
  bound, so an edited default or table keeps its old value. `makunbound` it
  before reloading. Hit twice here: a `night/llm-path-policy` missing a rule,
  and a `night/h-llm-scopes` missing a whole scope.
- `after!` bodies accumulate in `after-load-alist` — each load appends rather
  than replaces — so a later `provide` replays *every* historical body, and a
  definition you just deleted or renamed can come back. Almost every file here
  is wrapped in `after!`, so this is the normal case, not an edge one.
- `makunbound` does **not** retire a `defvaralias`. The old name still
  redirects, so `boundp` answers t again the moment a reload re-`defvar`s the
  target — and your assertion that the old names are gone fails while looking
  like the deletion never took.

So reload freely to exercise behaviour, but it is never evidence that something
is gone or updated. Confirm renames, deletions and `defvar` edits in
`emacs -Q --batch`, stubbing the few load-time macros the file needs
(`night/defface`) and asserting on `indirect-function` / `indirect-variable`.
Byte-compile the same file in batch while you are there: it catches free
variables, undefined functions, and "Alias for X should be declared before its
referent", which is a real bug for obsolete *variable* aliases — declared after
the new `defvar`, the new value wins and a customized old value is dropped.

### Retiring a name for good

`makunbound` on a `defvaralias` unbinds the **target**, so a value customized
at runtime on the real variable is lost. `(unintern sym obarray)` is what
actually retires a name: the alias record dies with the symbol object and the
target is untouched.

Measured while deleting the `night/fim-*` aliases: eight symbols survived a
clean that unbound all sixty-seven, and they were exactly the eight
`define-obsolete-variable-alias` sources.

To purge `after-load-alist` by hand, filter each entry's forms by
`prin1-to-string` against a regex for the old names, and **count what you
dropped** — that count is the only evidence the purge did anything. Note that
`getenv` inside the server reads the *server's* environment, so you cannot
label a run from the client side; when cleaning both servers, tell them apart
by their differing drop counts rather than by a tag you tried to pass in.

# Yasnippet Guidelines

Yasnippet includes everything after the `# --` marker in the snippet body, so a
trailing newline in the file becomes part of every expansion. Be deliberate
about it: end the file without a newline for inline snippets (e.g. timestamps),
with one when the expansion should end with a newline.

# Commit Guidelines

Never commit `./data/ispell-personal-dictionary`, as it might leak personal info. Tell the user to commit it themselves if need be.

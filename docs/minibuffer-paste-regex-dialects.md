# Pasting into a search minibuffer: which regex dialect wins

`s-,` in a minibuffer runs `night/pns` (`night/p-newline2space`). It does not
paste the clipboard as-is: since a minibuffer almost always means "search for
this", it downcases the text so smart-case matching applies, and quotes it so
the text matches itself rather than being read as a pattern.

Quoting is the hard part, because there is no single answer. The same
keystroke feeds four different consumers, and they do not agree on what a
backslash means.

## The bug this was written for

Pasting `\(\hat{t} = W^\top z + \bar{t}\)` into a `/` search failed outright —
not "found nothing", but `invalid-regexp "Invalid content of \{\}"`.

`regexp-quote` deliberately leaves `(`, `)`, `{`, `}`, `|` and `+` alone,
because they are literal in Emacs regexps; only their backslashed forms are
special. That is correct Emacs output. But Doom sets
`evil-ex-search-vim-style-regexp` to `t` (`modules/editor/evil/config.el`), so
evil ran the result through `evil-transform-vim-style-regexp` before searching,
and *that* rewrites exactly the characters `regexp-quote` left bare:

    paste:       \(\hat{t} = W^\top z + \bar{t}\)
    regexp-quote: \\(\\hat{t} = W\^\\top z \+ \\bar{t}\\)     ; valid Emacs regex
    evil:         \\(\\hat\{t\} = W\^\\top z \+ \\bar\{t\}\\) ; \{t\} is an interval
    result:       invalid-regexp

Braces error out. Parens and pipes are worse — they are silently reinterpreted
as groups and alternation, so the search quietly matches the wrong thing.

`autoload/evil/night-evil.el` now sets `evil-ex-search-vim-style-regexp` to
`nil`. Vim's magic mode spells groups, alternation and quantifiers `\(`, `\|`,
`\+` exactly as Emacs does, so what is lost is the `\d` `\s` `\w` class
shorthands and `\{n,m}` without the closing backslash. Doom's `:align` ex
commands call `evil-transform-vim-style-regexp` unconditionally and are
unaffected either way.

## The four consumers

**evil `/` and `?`** want an Emacs regexp, now that vim-style is off. They also
split the pattern from its search offset at the first unescaped `/`, so slashes
still need escaping — that branch in `night/regex-escape-smart` predates all of
this and is still needed.

**`consult-line`** goes through orderless, which treats each space-separated
component as an Emacs regexp. Spaces therefore mean "all of these, in any
order, on one line", so a pasted phrase is matched loosely rather than
contiguously. That is usually what you want when hunting for a paper title.

**`consult--grep` with ripgrep** (`night/search-dir` with `:engine "rg"`) also
wants an **Emacs** regexp, which is the counterintuitive one.
`consult--ripgrep-make-builder` calls `consult--compile-regexp`, which converts
Emacs syntax to PCRE itself and appends `-P`. Handing it a PCRE pattern would
be wrong. Do not test this by pasting the pattern into a shell `rg` — that
skips the conversion and will look broken when it is not.

**`night/consult-ugrep`** is the exception. Its builder passes the minibuffer
input to ugrep as `-e input`, unconverted, so the input must already be PCRE.
On top of that ugrep runs with `--bool`, where an unescaped space is an AND
operator, `|` is OR, and a leading `-` is NOT. `night/regex-escape-ugrep`
handles those: spaces become `[ ]`, a leading `-` is escaped, and `"` is
escaped because `--bool` reads a quoted run as a literal phrase.

## How the dispatch works

`night/h-regex-dialect` (`autoload/night-regex.el`) names the dialect the
current minibuffer's consumer expects — `emacs`, `pcre`, or `ugrep-bool`.
`night/regex-dialect` reads it, defaulting to `emacs`, which is right for
everything that lives inside Emacs.

Only `night/consult-ugrep` binds it, because it is the only entry point whose
engine sees the input raw. `night/regex-escape-smart` and
`night/regex-group-shy` both dispatch on it. Adding an engine means binding the
variable at its entry point; the escapers do not need to know about it.

`night/regex-group-shy` exists because grouping syntax is dialect-dependent
too, and this was a live bug: `night/h-arxiv-regex-canonizer` builds
`(?:<url>|<arxiv-id>)` so that a pasted arxiv link finds the bare ID as well,
and it used to hardcode the two spellings with a literal `"//"` where `"\\"`
was meant. Every non-ugrep search for an arxiv URL was therefore looking for a
pattern beginning `//(?:` and matched nothing.

## Known broken: `ivy-rg`

`night/search-dir` forces `:engine "ivy-rg"` for remote directories, and that
path cannot be fixed from the escaper. `counsel-rg` builds an Emacs regexp via
orderless and then converts it with `counsel--elisp-to-pcre`, which is lossy: a
literal backslash (`\\` in an Emacs regexp) comes out as a single `\`, which
then escapes whatever follows. Pasting `\hat{t}` yields

    \(\\hat{t}.*=.*w\^\\top.*z.*\+.*\\bar{t}\)

which ripgrep rejects — `repetition quantifier expects a valid decimal`. No
choice of input quoting survives that converter. Left unfixed and commented at
the call site.

## Testing this

Check the generated pattern against the real path, not a plausible-looking
approximation:

- evil: `(evil-ex-make-pattern PATTERN 'smart nil)`, then `string-match` the
  car against the original text.
- consult ripgrep: `(consult--join-regexps (car (consult--compile-regexp PATTERN 'pcre t)) 'pcre)`,
  then run *that* through `rg -P`.
- ugrep: the pattern goes to `ugrep --bool --perl-regexp` verbatim.

Write patterns to a file and use `rg -f` / `ugrep -f` rather than fighting
shell quoting; a mis-quoted test command looks exactly like a mis-quoted
pattern.

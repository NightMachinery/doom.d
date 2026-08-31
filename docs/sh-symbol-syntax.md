# The symbol at point in shell buffers stops at `:` and `%`

With point on `alert_color` inside `${alert_color:-red}`, `yio`
(`evil-inner-symbol`) used to yank `alert_color:-red` — the entire body of the
parameter expansion, operator and default value included.

## Why

`sh-mode-syntax-table` (`sh-script.el`) deliberately hands *symbol* syntax to
`! % : . ^ ~ ,`:

```elisp
(defvar sh-mode-syntax-table
  (sh-mode-syntax-table ()
        ...
        ?! "_"  ?% "_"  ?: "_"  ?. "_"  ?^ "_"  ?~ "_"  ?, "_"
        ?= "."  ?/ "."  ?\; "."  ?| "."  ?& "."  ?< "."  ?> "."))
```

`-` and `*` are not in that list; they get symbol syntax from the standard
table. So every one of `: % . - ^ ~ , !` glues to the identifier next to it,
and `${x:0:3}`, `${x%pat}`, `${x:h}` and `kill %emacs` all had the same problem.

This was never only a text-object bug. Everything that asks for "the symbol at
point" reads the same table:

- `yio` / `yao` — `evil-inner-symbol`, `evil-a-symbol` (`o` in the evil text
  object maps).
- `*` and `#` search — `evil-symbol-word-search` is `t` in this config.
- `M-.`, `company-dabbrev-code`.
- `night/brishz-doc-at-point` (`night-brish.el`), which passes
  `(symbol-at-point)` to `whichm` — it was looking up `alert_color:-red`.

## The fix

`night/sh-punctuation-chars` in `autoload/night-zsh.el` demotes `:` and `%` to
punctuation in `sh-mode-syntax-table` itself, under `after! sh-script`. Fixing
the table rather than the text object is what makes all five consumers above
correct at once.

`sh-mode` buffers share that table object — `(syntax-table)` is `eq` to
`sh-mode-syntax-table` — so mutating it in place reaches buffers that are
already open, and `bash-ts-mode` uses the same table. It also survives
`sh-set-shell`, which only calls `set-syntax-table` for shells listed in
`sh-mode-syntax-table-input`, and only `rpm` is listed there; zsh, bash and sh
keep the global table.

Why only these two:

- `:` is never part of a shell identifier. It is `${x:-y}`, `${x:=y}`,
  `${x:+y}`, `${x:?y}`, `${x:0:3}`, the zsh modifiers `${x:h}`, and the `PATH`
  separator.
- `%` is `${x%pat}`, `${x%%pat}`, job specs `%emacs`, and prompt escapes
  `%F{red}`.
- `-` and `.` are deliberately left alone, so zsh function names (`night-foo`),
  long flags (`--color`) and filenames (`a.txt`) stay single symbols.
- `! ^ ~ ,` are left alone too, but they are the obvious next candidates if
  `${x^^}`, `${x,,}` or `{a,b}` become annoying. Add them to the defvar.

Known cost: `ns::fn` namespaced function names now split at the colons. Worth
it — parameter expansion is far more common than that convention.

## The other end: `-` in `${x:-default}`

Fixing `:` only fixed the left side. Point on `plain` in

```zsh
    local markup="${alert_markup:-plain}" color="${alert_color:-}"
```

still yanked `-plain`, because `-` is a symbol constituent — and it has to stay
one, for `night-foo`, `--color` and `a-b`.

That is not something a syntax table can express. A table is
position-independent: it has to answer "symbol" for `night-foo` and
"punctuation" for `:-plain` with the same entry. Position-dependent syntax is
what `syntax-propertize` is for, so `night/sh-syntax-propertize-operators`
applies two rules as text properties:

- `:[-+=?]` — an operator directly after a colon: `${x:-y}`, `${x:+y}`,
  `${x:=y}`, `${x:?y}`. No shell construct has `:` followed directly by one of
  these with the pair belonging to a name. The property goes on the whole
  two-character match, since `:` is punctuation already.
- `${name-` — the colon-less POSIX forms `${x-y}`, `${x+y}`, `${x=y}`,
  `${x?y}`. Safe from the other direction: a shell variable name cannot contain
  `-`, so an operator directly after the name inside `${}` is never part of it.

`night/sh-syntax-propertize-setup-h` composes this onto sh-mode's own pass with
`add-function :after`, so `sh-syntax-propertize-function` keeps handling
heredocs and quoting. `syntax-propertize` sets `parse-sexp-lookup-properties`
itself (`syntax.el:400`), so `skip-syntax-forward` — and therefore
`forward-symbol`, `thing-at-point` and `evil-inner-symbol` — honour the
properties with no further setup.

Two things that are not optional, both learned the hard way:

**Flush on install.** If anything propertizes the buffer during mode setup,
before `sh-mode-hook` runs, `syntax-propertize--done` is already at
`point-max` and `syntax-propertize` never revisits the buffer — so the rules
silently never fire. This showed up as the same file behaving differently on
different opens, which is the worst way for it to fail. The setup ends with
`(syntax-ppss-flush-cache (point-min))`.

**Reach buffers that are already open.** Unlike the table mutation, a mode hook
only affects buffers created afterwards, so a long-lived shell buffer kept the
old behaviour across a reload and read as the change not working. The file ends
by walking `buffer-list` and running the setup in every `sh-mode` buffer.

Chunk boundaries are *not* a hazard here, despite the patterns being only two
or three characters wide: `syntax-propertize-extend-region-functions` defaults
to `(syntax-propertize-wholelines)` (`syntax.el:79`), so a rule can never be
split across two propertize passes.

Being local rather than structural is a feature. Rule one anchors on `:` and
ignores everything before it, which is why all of these work:

```
${x:-${y:-z}}       -> z         nesting
${p:-a-b}           -> a-b       dash inside the default, preserved
${q:-$(cmd --flag)} -> --flag    command substitution default
${arr[1]:-n}        -> n         subscripted name
${(f)v:-w}          -> w         zsh expansion flags
```

## Why not tree-sitter

Considered for the `-` problem and rejected:

- There is no zsh grammar, only `tree-sitter-bash`. Of the 452 `.zsh` files in
  `~/scripts`, 146 use `${(f)...}` expansion flags and 72 use `} always {`;
  both are parse errors for a bash grammar, and inside an ERROR node the tree
  is unusable. The feature would silently degrade across a third of the corpus.
- The grammar is not even installed — `(treesit-ready-p 'bash)` is nil, and only
  `libtree-sitter-python.dylib` is present — so it would add a per-machine
  compile step to a config that syncs to the VPS. `sh--redirect-bash-ts-mode`
  also sends non-bash/sh shells back to `sh-mode`, so `.zsh` would never reach
  `bash-ts-mode` anyway without overriding that deliberately.
- It would fix `yio` only. `*` search, `M-.`, `company-dabbrev-code` and
  `night/brishz-doc-at-point` read syntax, not a parse tree.
- It does not win on robustness either — see the `${(f)v:-w}` line above, which
  a bash parser could not read at all.

Tree-sitter is still the right tool for *structural* objects — inner function, a
`case` branch, a pipeline — and `evil-textobj-tree-sitter` is already pinned in
`bootstrap/straight-versions.el`. That is a separate feature, and it would still
need a grammar that can read zsh.

## The risk that had to be checked

`sh-mode` indents through SMIE, and `sh-smie--default-forward-token` tokenizes
by syntax class:

```elisp
(if (zerop (skip-syntax-forward "."))
    (while (progn (skip-syntax-forward "w_'") ...)))
```

Moving `:` and `%` from `w_` to `.` therefore changes how shell source is
tokenized, which could in principle move indentation. Measured rather than
assumed: 100 real `.zsh`/`.sh`/`.bash` files under `~/scripts` were reindented
with `indent-region` under the old table and the new one and the buffers
compared. Zero differences. The same check was run again for the
`syntax-propertize` rules, since those change syntax classes too — also zero.

Re-run that check if the char list or the rules are ever extended. For the
table, compare against a copy with the chars restored; for the rules, compare a
plain `sh-mode` buffer against one with `night/sh-syntax-propertize-setup-h`
called:

```elisp
(let* ((files (seq-take (directory-files-recursively "~/scripts" "[.]\\(zsh\\|sh\\|bash\\)$") 100))
       (base (make-syntax-table sh-mode-syntax-table)))
  (dolist (c night/sh-punctuation-chars) (modify-syntax-entry c "_" base))
  (seq-filter
   (lambda (f)
     (not (equal
           (with-temp-buffer (insert-file-contents f) (delay-mode-hooks (sh-mode))
                             (set-syntax-table base)
                             (indent-region (point-min) (point-max)) (buffer-string))
           (with-temp-buffer (insert-file-contents f) (delay-mode-hooks (sh-mode))
                             (indent-region (point-min) (point-max)) (buffer-string)))))
   files))
```

## Checking the behaviour

```elisp
(with-temp-buffer
  (sh-mode)
  (night/sh-syntax-propertize-setup-h)
  (insert "x=${alert_color:-red} ; foo-bar --color a.txt ${y:0:3} ${z%.zsh}")
  (syntax-propertize (point-max))
  (goto-char 6)
  (thing-at-point 'symbol))
;; => "alert_color"
```

A temp buffer does not exercise the path that actually matters, though — the
properties come from font-lock driving `syntax-propertize`. Check a real one:

```elisp
(let ((buf (find-file-noselect "some.zsh")))
  (with-current-buffer buf
    (font-lock-ensure)
    (goto-char (point-min))
    (re-search-forward ":-[A-Za-z_]")
    (goto-char (1- (point)))
    (thing-at-point 'symbol)))
```

`foo-bar`, `--color` and `a.txt` stay whole; `${y:0:3}` gives `y` and
`${z%.zsh}` gives `z`.

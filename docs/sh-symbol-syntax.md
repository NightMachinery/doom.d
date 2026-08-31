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
compared. Zero differences.

Re-run that check if the char list is ever extended:

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
  (insert "x=${alert_color:-red} ; foo-bar --color a.txt ${y:0:3} ${z%.zsh}")
  (goto-char 6)
  (thing-at-point 'symbol))
;; => "alert_color"
```

`foo-bar`, `--color` and `a.txt` stay whole; `${y:0:3}` gives `y` and
`${z%.zsh}` gives `z`.

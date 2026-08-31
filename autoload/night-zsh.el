;;; autoload/night-zsh.el -*- lexical-binding: t; -*-

(defvar night/sh-punctuation-chars '(?: ?%)
  "Chars demoted from symbol to punctuation in `sh-mode-syntax-table'.

These are shell expansion operators, never parts of an identifier, but
`sh-script' hands them symbol syntax, so the symbol at point in
=${alert_color:-red}= comes out as =alert_color:-red=.  That hits `yio'
\(`evil-inner-symbol'), `*' search (`evil-symbol-word-search' is t),
`M-.', `company-dabbrev-code' and `night/brishz-doc-at-point' alike.

Deliberately not demoted: `-' and `.', so that zsh function names
\(=night-foo=), long flags (=--color=) and filenames (=a.txt=) stay whole.
Add `?! ?^ ?~ ?,' here if =${x^^}=, =${x,,}= or ={a,b}= start to annoy.

The cost of demoting `:' is that =ns::fn= namespaced function names split
at the colons.")

(after! sh-script
  ;; `sh-mode' buffers share this table object (`(syntax-table)' is `eq' to it),
  ;; and `sh-set-shell' only replaces it for shells listed in
  ;; `sh-mode-syntax-table-input' (only =rpm= is), so mutating it in place reaches
  ;; open buffers too and survives a re-mode.
  (dolist (c night/sh-punctuation-chars)
    (modify-syntax-entry c "." sh-mode-syntax-table)))

(defun night/sh-syntax-propertize-operators (start end)
  "Give punctuation syntax to expansion operators glued onto a name.

`-' cannot go in `night/sh-punctuation-chars': it has to stay a symbol
constituent for =night-foo=, =--color= and =a-b=.  Position by position it
can be told apart, which is what `syntax-propertize' is for.  Without this,
point on =plain= in =${alert_markup:-plain}= yanks =-plain=.

Two rules, both unambiguous rather than heuristic:

- =:[-+=?]=, an operator directly after a colon.  That is =${x:-y}=,
  =${x:+y}=, =${x:=y}=, =${x:?y}=; no construct has =:= followed directly by
  one of these with the pair belonging to a name.  The syntax goes on the
  whole match because `:' is punctuation already, so there is nothing to
  preserve.

- =${name-=, the colon-less POSIX forms.  Safe from the other direction: a
  shell variable name cannot contain `-', so an operator directly after the
  name inside =${}= is never part of it.

Being local rather than structural is a feature here -- rule one is anchored
on `:' and ignores what precedes it, so =${(f)v:-w}= works even though no
bash parser can read zsh expansion flags.  @seeAlso docs/sh-symbol-syntax.md"
  (funcall
   (syntax-propertize-rules
    (":[-+=?]" (0 "."))
    ("\\${[#!]?[[:alnum:]_]+\\([-+=?]\\)" (1 ".")))
   start end))

(defun night/sh-syntax-propertize-setup-h ()
  "Compose `night/sh-syntax-propertize-operators' onto sh-mode's own pass.
`add-function' rather than `setq-local', so `sh-syntax-propertize-function'
keeps handling heredocs and quoting.  It is idempotent, so re-running the
hook does not stack copies.

Unlike the syntax table, this only reaches buffers that enter `sh-mode'
after this file is loaded; revert an already-open one to pick it up.

The flush is not optional.  Anything that propertizes during mode setup --
before this hook runs -- leaves `syntax-propertize--done' at the end of the
buffer, and `syntax-propertize' then never revisits it, so the rules would
silently never fire.  That failed intermittently before the flush, which is
the worst way for it to fail."
  (add-function :after (local 'syntax-propertize-function)
                #'night/sh-syntax-propertize-operators)
  (syntax-ppss-flush-cache (point-min)))

(add-hook 'sh-mode-hook #'night/sh-syntax-propertize-setup-h)

(dolist (buf (buffer-list))
  ;; So that reloading this file reaches shell buffers that are already open,
  ;; the way mutating `sh-mode-syntax-table' does. Without it a long-lived
  ;; buffer keeps the old behaviour until it is reverted, which reads as the
  ;; change not working. Idempotent: `add-function' replaces its own entry.
  (with-current-buffer buf
    (when (derived-mode-p 'sh-mode)
      (night/sh-syntax-propertize-setup-h))))

(defun night/zsh-mode-startup ()
  (interactive)
  (outline-minor-mode)
  (setq-local outline-regexp "##")

  ;;;
  ;; (setq-local hl-todo-keyword-faces hl-todo-keyword-faces)
  ;; (push
  ;;  `(
  ;;    ;; night/at-tag-regex
  ;;    ,(concat "@" night/at-tag-char-regex "+")
  ;;    ;; ,(concat "@" night/at-tag-main-char-regex "+")
  ;;    at-tag-face)
  ;;  hl-todo-keyword-faces)
  ;;;

  (set-company-backend! 'sh-mode '(company-dabbrev-code company-files)))

(add-hook 'sh-mode-hook #'night/zsh-mode-startup)

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

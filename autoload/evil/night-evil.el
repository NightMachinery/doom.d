;;; ~/doom.d/night-evil.el -*- lexical-binding: t; -*-

;; (evilem-default-keybindings "s")      ; #easymotion
  (setq evil-want-fine-undo t)

;; (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
;; (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
(setq evil-cross-lines t)
(setq evil-move-beyond-eol t)
(setq evil-move-cursor-back nil)
;;;
;; Doom turns this on (=modules/editor/evil/config.el=), but we do not use Vim
;; regex syntax, and having it on adds a third dialect that everything we paste
;; into a `/' search has to be escaped for: `evil-transform-vim-style-regexp'
;; rewrites the unescaped `{' `}' `(' `)' `|' `+' that `regexp-quote'
;; deliberately leaves alone. Pasting LaTeX such as =\hat{t}= turned the braces
;; into an interval operator and the search died with "Invalid content of \{\}".
;; With this nil, evil reads plain Emacs regexes, same as everything else in
;; Emacs. We lose Vim's `\d' `\s' `\w' class shorthands; groups, alternation and
;; quantifiers are spelled `\(' `\|' `\+' either way.
;; @seeAlso `night/regex-escape-smart', `night/h-regex-dialect'
(setq evil-ex-search-vim-style-regexp nil)
;;;

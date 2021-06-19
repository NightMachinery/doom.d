;;; autoload/night-commonlisp.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("\\.\\(sbclrc\\)\\'" . lisp-mode))

(after! (sly)
  (setq sly-description-autofocus t))

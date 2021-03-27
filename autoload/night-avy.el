;;; autoload/night-avy.el -*- lexical-binding: t; -*-

(after! avy

  (defun night/avy-goto-opening-paren ()
    (interactive)
    (avy-jump "[[{(]"))
  (defun night/avy-goto-closing-paren ()
    (interactive)
    (avy-jump "[])}]"))
  ;;;
  (defun night/avy-goto-org-header ()
    (interactive)
    (when (avy-jump "^\*+ ")
      (org-back-to-heading)))
  )

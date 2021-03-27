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
    ;; https://github.com/abo-abo/avy/issues/320
    (let ((avy-text ""))
      (when (avy-jump "^\*+ ")
        ;; (when avy-text (forward-char (length avy-text)))
        )))
  )

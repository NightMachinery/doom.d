;;; autoload/night-zsh-wrappers.el -*- lexical-binding: t; -*-
;;;
(defun night/p-org-fanfic ()
  (interactive)
  (save-excursion
    (insert-for-yank (z p-org-fanfic)))
  (recenter 0)
  (save-buffer))
;;;

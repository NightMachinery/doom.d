;;; autoload/night-zsh-wrappers.el -*- lexical-binding: t; -*-
;;;
(defun night/p-org-fanfic ()
  (interactive)
  (night/insert-for-yank-and-save
   (z p-org-fanfic))
  (night/bell-link))
;;;
(defun night/semantic-scholar-to-org ()
  (interactive)
  (night/insert-for-yank-and-save
   (z semantic-scholar-to-org))
  (night/bell-link))
;;;

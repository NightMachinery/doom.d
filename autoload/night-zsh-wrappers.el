;;; autoload/night-zsh-wrappers.el -*- lexical-binding: t; -*-
;;;
(defun night/p-org-fanfic ()
  (interactive)
  (night/insert-for-yank-and-save
   (z p-org-fanfic))
  (night/bell-link))
;;;
(defun night/p-newline2space ()
  (interactive)
  (night/insert-for-yank
   (z p-newline2space)))
(defalias 'night/pns #'night/p-newline2space)
;;;
(defun night/semantic-scholar-to-org ()
  (interactive)
  (night/insert-for-yank-and-save
   (z semantic-scholar-to-org))
  (night/bell-link))
;;;

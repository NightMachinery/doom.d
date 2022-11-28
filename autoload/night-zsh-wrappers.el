;;; autoload/night-zsh-wrappers.el -*- lexical-binding: t; -*-
;;;
(defun night/org-link-browser-current ()
  (interactive)
  (night/insert-for-yank-and-save
   (z org-link-browser-current)))
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
  (night/org-insert-and-fix-levels
   (z semantic-scholar-to-org))
  (night/save-buffer)
  (night/bell-link))
;;;
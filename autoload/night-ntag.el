;;; autoload/night-ntag.el -*- lexical-binding: t; -*-

(defvar ntag-sep "..")

(defun night/ntag-create (tag)
  (concat  ntag-sep tag ntag-sep))
(defalias 'ntag #'night/ntag-create)
(comment
 (ntag "hi"))

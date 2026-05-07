;;; autoload/night-string.el -*- lexical-binding: t; -*-

(defun night/escape-spaces-with-backslash (string)
  "Escape spaces in STRING with backslashes."
  (replace-regexp-in-string " " "\\\\ " string))

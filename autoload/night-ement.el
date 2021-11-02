;;; autoload/night-ement.el -*- lexical-binding: t; -*-

(defun night/ement-connect ()
  (interactive)
  (ement-connect
   :user-id "@fereidoon:matrix.org"
   :uri-prefix "http://matrix.org"))

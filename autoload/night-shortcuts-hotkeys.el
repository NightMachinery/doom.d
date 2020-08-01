;;; ~/doom.d/autoload/night-shortcuts-hotkeys.el -*- lexical-binding: t; -*-

(defun night/tmp-buffer ()
  (interactive)
  (find-file "~/tmp/tmp.txt"))

(night/set-leader-keys " z t" #'night/tmp-buffer)

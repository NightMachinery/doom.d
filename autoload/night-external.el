;;; autoload/night-external.el -*- lexical-binding: t; -*-

(defun night/path-unabbrev (path)
  (z path-unabbrev (identity path)))
(comment
 (night/path-unabbrev "~mu/hi.mp3"))

(defun night/ensure-dir (path)
  (when (zb ensure-dir (identity path))
   path))

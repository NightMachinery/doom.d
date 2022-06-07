;;; autoload/night-external.el -*- lexical-binding: t; -*-

(defun night/path-unabbrev (path)
  ;;;
  ;; @workaround for the lack of support of non-utf-8 in brish
  (eredis-set "emacs_input" path)
  (z eval (concat "path-unabbrev \"$(redism get emacs_input)\""))
  ;;;
  ;; (z path-unabbrev (identity path))
  ;;;
  )
(comment
 (night/path-unabbrev "~mu/hi.mp3"))

(defun night/ensure-dir (path)
  (when (zb ensure-dir (identity path))
   path))

;;; autoload/night-external.el -*- lexical-binding: t; -*-
;;;
(require 'memoize)
;;;
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

(defun night/path-abbrev (path)
;;;
  ;; @workaround for the lack of support of non-utf-8 in brish
  (eredis-set "emacs_input" path)
  (z eval (concat "path-abbrev \"$(redism get emacs_input)\"")))
(comment
 (night/path-abbrev "/Users/evar/my-music/hi.mp3"))

(defun night/path-abbrev-memoized (&rest args)
  (apply #'night/path-abbrev args))

(memoize #'night/path-abbrev-memoized "9999 hours")
;;;
(defun night/ensure-dir (path)
  (when (zb ensure-dir (identity path))
    path))
;;;

;;; autoload/org/night-org-export.el -*- lexical-binding: t; -*-
;;;
(defun night/org-export-string-as-utf8 (str)
  "Assume str has Org syntax, and convert it to UTF-8."
  (interactive)
  (let ((org-ascii-charset 'utf-8))
    (org-export-string-as
     str 'ascii t)))
(comment
 (night/org-export-string-as-utf8 "*wow* /hi/ you")
 ;; yes, doesn't quite work as expected
 )
;;;

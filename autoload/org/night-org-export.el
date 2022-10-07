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
(defun night/h-org-export-preprocess-add-default-setupfiles (backend)
  (when (eq backend 'html)
    (goto-char 0)
    (insert "#+SETUPFILE: https://nightmachinery.github.io/orgmode-styles/notes_1.org\n")))

(add-hook 'org-export-before-processing-hook #'night/h-org-export-preprocess-add-default-setupfiles)
;;;
(defun night/org-export-file-to-html (file)
  (save-current-buffer
    (let ((b (find-file-noselect file)))
      (set-buffer b)
      (let
          ((out (org-html-export-to-html)))
        (message "out: %s" out)
        (if (f-absolute-p out)
            out
          (concat default-directory out))))))
(comment
 (night/org-export-file-to-html "/Users/evar/cellar/notes/subjects/math/AI/ML/NLP/learn/courses/cs224N/gen.org"))
;;;

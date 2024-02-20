;;; autoload/org/night-org-util.el -*- lexical-binding: t; -*-
;;;
(defun night/org-current-level ()
  (interactive)
  (or (org-current-level) 0))
;;;
(defun night/org-str-to-plain (str)
  ;; @Q&A Does org have a function to convert an org-mode formatted string to a plain text string without the rich formatting? Sth like `(org-strip "*bold* /italic/") == "bold italic"`
  ;;
  ;; It seems using regexes is the best idea, as simple markup tags such as =*bold*= don't support escape chars (=\*=) any way
;;;
  (z reval-withstdin (i str) org2plain /dev/stdin))
(comment
 (night/org-str-to-plain "** *wow* /hi/ [[url][desc]]"))
;;;
(defun night/current-line-level ()
  "@seeAlso [help:org-current-level]"
  (interactive)
  (cl-parse-integer (z
                     reval-withstdin
                     (helm-current-line-contents)
                     perl -lne "m/^(\\**)/ && print length $1")))
;;;

;;; ~/doom.d/autoload/org/templates.el -*- lexical-binding: t; -*-

;;;
;; The new API is very limited and can only insert #+BEGIN_X and #+END_X
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sj" . "src julia"))
;;;

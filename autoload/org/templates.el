;;; ~/doom.d/autoload/org/templates.el -*- lexical-binding: t; -*-

;;;
;; The new API is very limited and can only insert #+BEGIN_X and #+END_X
(require 'org-tempo)
;; (add-to-list 'org-structure-template-alist '("sj" . "src julia"))
(add-to-list 'org-structure-template-alist '("sj" . "src jupyter-julia :session j1 :wrap example"))
(add-to-list 'org-structure-template-alist '("sz" . "src bsh.dash :results verbatim :exports both :wrap example"))
(add-to-list 'org-structure-template-alist '("sb" . "src bash :results verbatim :exports both :wrap example"))
(add-to-list 'org-structure-template-alist '("sp" . "src python :session p1 :results value :exports both :wrap example"))
(add-to-list 'org-structure-template-alist '("spo" . "src python :session p1 :results output :exports both :wrap example"))
;;;

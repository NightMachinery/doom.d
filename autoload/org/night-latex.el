;;; autoload/org/night-latex.el -*- lexical-binding: t; -*-

 ;; You can adapt the old code at http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/ to automatically change the previews to code and vice versa when the cursor enters/leaves them.
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-default-process 'dvisvgm)
;;;

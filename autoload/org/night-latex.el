;;; autoload/org/night-latex.el -*- lexical-binding: t; -*-

(after! org
  ;; You can adapt the old code at http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/ to automatically change the previews to code and vice versa when the cursor enters/leaves them.
  ;; Update: we already have automatic previews ...
  ;;;
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; https://emacs.stackexchange.com/questions/19880/font-size-control-of-latex-previews-in-org-files
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))
  (setq org-format-latex-options (plist-put org-format-latex-options :foreground "black")))

(after! (tex-mode)
  (setq tex-indent-arg 2))

;;; autoload/night-transclusion.el -*- lexical-binding: t; -*-
(use-package! org-transclusion
  :after org
  :init
  (progn
    (defun night/org-transclusion-refresh-all ()
      (interactive)
      (org-transclusion-remove-all)
      (org-transclusion-add-all))

    (map!
     ;; :map global-map "<f12>" #'org-transclusion-add
     :leader
     "n t" #'org-transclusion-mode
     "n n" #'night/org-transclusion-refresh-all
     "n b" #'org-transclusion-remove-all
     "n m" #'org-transclusion-add-all
     )
    (setq org-transclusion-exclude-elements nil)))

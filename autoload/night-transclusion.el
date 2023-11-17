;;; autoload/night-transclusion.el -*- lexical-binding: t; -*-
(use-package! org-transclusion
  :after org
  :init
  (progn
    (map!
     ;; :map global-map "<f12>" #'org-transclusion-add
     :leader
     :prefix "n"
     :desc "Org Transclusion Mode" "t" #'org-transclusion-mode)
    (setq org-transclusion-exclude-elements nil)))

;;; autoload/night-helm.el -*- lexical-binding: t; -*-

(after! (helm)
  (define-key helm-map (kbd "M-<up>") #'helm-previous-page)
  (define-key helm-map (kbd "M-<down>") #'helm-next-page))

;;; autoload/org/night-startup.el -*- lexical-binding: t; -*-

(defun night/org-startup ()
  (interactive)
  (org-fragtog-mode)  ;; https://github.com/io12/org-fragtog

  (night/highlight-background)        ;; @futureCron was this worth overcoming hl-line?
  )

(add-hook 'org-mode-hook #'night/org-startup)

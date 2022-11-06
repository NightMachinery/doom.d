;;; autoload/org/night-startup.el -*- lexical-binding: t; -*-

(defun night/org-startup ()
  (interactive)
  (org-fragtog-mode) ;; https://github.com/io12/org-fragtog

  (comment
   (org-roam-mode -1)
 ;; @workaround There are suddenly nonexistent roam commands being mapped to keys. This is probably a Doom regression which will go away with time.
 ;; @update This only worked with roam/v1. I had to completely disable roam/v2 to get rid of these problems.
   )

  (night/highlight-background) ;; @futureCron was this worth overcoming hl-line?
  (night/org-company-backends-set)
  (night/disable-company-frontends)

  (comment
   (map! :map 'local
         :ing
         "TAB" #'org-cycle ;; trying to prevent =company= from hijacking these
         "<tab>" #'org-cycle
         [tab] #'org-cycle
         )))

(add-hook 'org-mode-hook #'night/org-startup)

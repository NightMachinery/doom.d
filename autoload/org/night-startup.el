;;; autoload/org/night-startup.el -*- lexical-binding: t; -*-

(defun night/org-startup ()
  (interactive)
  (org-fragtog-mode) ;; https://github.com/io12/org-fragtog

  (when (display-graphic-p)
    (night/disable-line-numbers) ;; @?
    )

  (when (display-graphic-p)
    (setq-local scroll-margin 0)
    ;; It makes scrolling with images bad.
    )
  (comment
   (org-roam-mode -1)
   ;; @workaround There are suddenly nonexistent roam commands being mapped to keys. This is probably a Doom regression which will go away with time.
   ;; @update This only worked with roam/v1. I had to completely disable roam/v2 to get rid of these problems.
   )

  (when (fboundp #'night/org-show-link-display)
    (night/org-show-link-display))
  (night/highlight-background) ;; @futureCron was this worth overcoming hl-line?
  (night/org-company-backends-set)
  (night/disable-company-frontends)

  (run-with-timer
   0.1
   nil
   (lambda ()
     (interactive)
     (night/babel-ansi-all2)))
  ;; (xterm-color-colorize-buffer)

  (comment
   (map! :map 'local
         :ing
         "TAB" #'org-cycle ;; trying to prevent =company= from hijacking these
         "<tab>" #'org-cycle
         [tab] #'org-cycle))
;;;
  (add-hook 'after-save-hook #'night/org-save-hook-fn)
;;;
  )

(add-hook 'org-mode-hook #'night/org-startup)

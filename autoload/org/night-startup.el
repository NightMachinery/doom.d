;;; autoload/org/night-startup.el -*- lexical-binding: t; -*-

(after! (org)
  (modify-syntax-entry ?= "." org-mode-syntax-table)
  (modify-syntax-entry ?/ "." org-mode-syntax-table)
  (modify-syntax-entry ?* "." org-mode-syntax-table)

  (modify-syntax-entry ?@ "w" org-mode-syntax-table)
  ;; (modify-syntax-entry ?@ "w" org-mode-tags-syntax-table)
  )

(defvar *night/h-interactive-buffer-force* nil
  "Dynamically bind this to t to force `night/interactive-buffer-p' to return true.")

(defun night/interactive-buffer-p ()
  ;; [[id:86053cea-abb2-43fb-b1d9-8bec1b93286c][elisp: hook: check if buffer has been opened interactively]]
  (or
   *night/h-interactive-buffer-force*
   (window-live-p (get-buffer-window (current-buffer) 'visible))))

(defun night/org-interactive-startup ()
  (interactive)
  (when (display-graphic-p)
    (org-fragtog-mode)
    ;; https://github.com/io12/org-fragtog

    (night/org-latex-preview-buffer))

  (when (fboundp #'night/org-show-link-display)
    ;; Only call if `#+STARTUP: descriptivelinks` option is not in the file:
    ;; Update: It seems this is no longer needed.
    ;; Even with this disabled, the above startup option does not correctly set [help:org-link-descriptive].
    (when nil
      (night/org-show-link-display)))
  (night/highlight-background)

  (run-with-timer
   0.1
   nil
   (lambda ()
     (interactive)
     (setq-local evil-shift-width 4)
     ;; Something keeps overriding our `evil-shift-width', so I am setting it here, too.

     (night/babel-ansi-all2))))

(defun night/org-startup ()
  (interactive)
  (night/h-yas-rm-snippets)
  ;; @hack Somehow the snippets get through, so I just remove them again on org startup.

  (comment
   ;; run in [help:night/file-extension-actions] instead
   (when
       ;; t
       (night/interactive-buffer-p) ;; not reliable
     (night/org-interactive-startup)
     ))

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

  (night/org-company-backends-set)
  (night/disable-company-frontends)

  ;; (setq tab-width 4)
  (setq-local evil-shift-width 4)
  ;; [[id:f4799b9b-fa11-46cd-b402-cc8f0531aca0][orgmode/indent]]

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

(add-hook
 'org-mode-hook
 ;; 'evil-org-mode-hook
 #'night/org-startup)

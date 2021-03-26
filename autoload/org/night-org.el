;;; Don't name this file org.el, emacs will think it's the actual org mode and things will break.
(after! org
  (setcdr org-link-abbrev-alist
          `(
            ("NIGHTDIR" . ,(concat (getenv "NIGHTDIR") "/"))
            ("cellar" . ,(concat (getenv "cellar") "/"))
            ("nightNotes" . ,(concat (getenv "nightNotes") "/"))
            ("orgdir" . ,(concat  org-directory "/"))))
  (add-to-list 'org-modules 'org-protocol)
;;;
  (add-hook 'org-mode-hook 'org-fragtog-mode) ; https://github.com/io12/org-fragtog
  ;; You can adapt the old code at http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/ to automatically change the previews to code and vice versa when the cursor enters/leaves them.
  (setq org-startup-with-latex-preview t)
  (setq org-preview-latex-default-process 'dvisvgm)
;;;
  (setq org-return-follows-link t)
;;;
  (setq org-babel-min-lines-for-block-output 0) ; If number of lines of output is equal to or exceeds thisvalue, the output is placed in a #+begin_example...#+end_exampleblock.
  (setq org-startup-with-inline-images t)
  ;; (setq org-src-tab-acts-natively nil) ; doesn't fix the completion TAB problem
  (defun night/org-save-hook-fn ()
    (interactive)
    (when (string= "org-mode" major-mode)
      ;; (org-html-export-to-html)
      (org-babel-tangle)
      ))
  (add-hook 'after-save-hook #'night/org-save-hook-fn)
;;;
  (setq org-image-actual-width '(700)) ; this zooms small images though and downscales big ones. It unfortunately overrides per-image attribute settings.
  ;; (setq org-image-actual-width '(fill-column))
;;;
  (defun night/org-paste-clipboard-image ()
    "Paste the image in the clipboard at point."
    (interactive)
    ;; (org-display-inline-images)
    (setq filename
          (concat
           (make-temp-name
            (concat (file-name-nondirectory (buffer-file-name))
                    "_imgs/"
                    (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
                                        ; take screenshot
    (if (eq system-type 'darwin)
        ;; url-copy-file for downloading URLs
        ;; (call-process "pngpaste" nil nil nil filename)
        ;; @bug This always uses the png extension, while the file can be, e.g., jpg.
        (call-process "brishzq.zsh" nil nil nil "saveas-img" (concat (file-name-directory (buffer-file-name)) "/" filename))
      ;; (call-process "screencapture" nil nil nil "-i" filename)
      )
    (if (eq system-type 'gnu/linux)
        (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken
    (if (file-exists-p filename)
        (insert (concat "[[file:" filename "]]")))
    (org-redisplay-inline-images)
    )

  (setq org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . nil)))

  (map! :map org-mode-map
        :localleader
        :nvi "lp" #'night/org-paste-clipboard-image
        )
;;;
  )

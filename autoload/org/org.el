
(setq org-image-actual-width 600) ; this zooms small images though and downscales big ones. It unfortunately overrides per-image attribute settings.
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
      (call-process "pngpaste" nil nil nil filename)
      ;; (call-process "screencapture" nil nil nil "-i" filename)
      )
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "]]"))))
;;;

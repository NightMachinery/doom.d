;;; autoload/org/links/night-ol.el -*- lexical-binding: t; -*-

(after! (org)
  (add-to-list 'auto-mode-alist '("\\.\\(ol\\)\\'" . org-mode))

  (defcustom night/ol-enabled t
    "Whether to follow .ol links"
    :type '(choice (const :tag "disabled" nil)
                   (const :tag "enabled" t)))

  (defun night/ol-follow (&optional buffer)
    (interactive)
    (when night/ol-enabled
      (let ((buffer
             (or buffer
                 (current-buffer))))
        (let ((ol-link (buffer-substring-no-properties (point-min) (point-max))))

          (when (ignore-errors
                  (string=
                   (substring-no-properties ol-link 0 2)
                   "[["))               ;; the rigidity here will hopefully help us avoid acting on files that are only coincidentally named as org-link files.
            (org-link-open-from-string ol-link)
            (kill-buffer buffer) ;; so that `find-file-hook' is run again if it is opened
            )))))
  )

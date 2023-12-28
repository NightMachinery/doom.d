;;; night-org-links-zshfile.el ---                   -*- lexical-binding: t; -*-
(after! (org ol)
  (defun night/org-link-zshfile-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
;;;
    (let* ((expanded-path
            (night/path-unabbrev path)))
      (org-link-open-as-file expanded-path arg)))

  (org-link-set-parameters "zf" :follow #'night/org-link-zshfile-follow)
;;;
  (org-link-set-parameters "audiofile" :follow #'night/org-link-zshfile-follow)

  (cl-defun night/audiofile-link-get-current-line (&key (open-link-p nil))
    "Check if the current line contains an 'audiofile' link and return the path.

If OPEN-LINK-P is non-nil, open the link using Org functions."

    (interactive)
    (save-excursion

      (let* ((line-text (night/current-line-get))
             (link-regex "\\[audiofile:\\(\\(?:\\\\\\]\\|[^]]\\)+\\)\\]"))
        (when (string-match link-regex line-text)
          (let ((link-path (match-string 1 line-text)))
            (when open-link-p
              (org-open-link-from-string (format "[[audiofile:%s]]" link-path)))
            link-path)))))
;;;
  )

;;; night-org-links-file-point.el ---                   -*- lexical-binding: t; -*-
(after! (org ol)
  (defun night/org-link-file-point-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
    ;;;
    (let* ((opts (s-split "::" path))
           (path (car opts))
           (goal-point (string-to-number (cadr opts))))
      (org-link-open-as-file path arg)
      (goto-char goal-point)))

  (org-link-set-parameters "file_point" :follow #'night/org-link-file-point-follow)
  )

;;; night-org-links-file-point.el ---                   -*- lexical-binding: t; -*-
(after! (org ol)
  (defun night/org-link-file-point-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
;;;
    (let* ((opts (s-split "::" path))
           (path (car opts))
           (goal-point (string-to-number (cadr opts))))
      (org-link-open-as-file path arg)
      (night/goto-byte goal-point)
      (night/org-reveal-maybe)))

  (org-link-set-parameters "file_point" :follow #'night/org-link-file-point-follow)
  )

(defun night/goto-byte (byte-position)
  (interactive "NGoto position: ")
  (let ((multibyte-orig enable-multibyte-characters))
    (set-buffer-multibyte nil) ;; @hack to allow us to go to the nth byte (instead of the nth char)
    (with-demoted-errors
        (goto-char byte-position))
    (set-buffer-multibyte multibyte-orig)))

(defun night/org-reveal-maybe (&rest args)
  (interactive)
  (when (equalp major-mode 'org-mode)
    (apply #'org-reveal args)))

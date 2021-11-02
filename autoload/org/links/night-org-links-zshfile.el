;;; night-org-links-zshfile.el ---                   -*- lexical-binding: t; -*-
(after! (org ol)
  (defun night/org-link-zshfile-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
    ;;;
    (let* ((expanded-path
            (night/path-unabbrev path)))
      (org-link-open-as-file expanded-path arg)))

  (org-link-set-parameters "zf" :follow #'night/org-link-zshfile-follow)
  (org-link-set-parameters "audiofile" :follow #'night/org-link-zshfile-follow))

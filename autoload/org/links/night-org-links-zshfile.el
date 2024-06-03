;;; night-org-links-zshfile.el ---                   -*- lexical-binding: t; -*-
(after! (org ol)
  (defun night/org-link-zshfile-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
    (let* ((expanded-path
            (night/path-unabbrev path)))
      (org-link-open-as-file expanded-path arg)))

  (org-link-set-parameters "zf" :follow #'night/org-link-zshfile-follow)
;;;
  (defun night/org-link-open-zf-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
    (let* ((expanded-path
            (night/path-unabbrev path)))
      (z-async t
               open (identity expanded-path))))

  (org-link-set-parameters "open-zf" :follow #'night/org-link-open-zf-follow)
;;;
  (provide 'night-org-zshfile)
)

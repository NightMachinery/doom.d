;;; night-org-links-zshfile.el ---                   -*- lexical-binding: t; -*-
(after! (org ol)
  (defun night/org-link-zshfile-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
    (let* ((expanded-path
            (night/path-unabbrev path)))
      ;; `org-open-file' skips its own existence check whenever `org-file-apps'
      ;; resolves to `emacs' ("Emacs has no problems with non-ex files"), and
      ;; ours resolves everything that way via (t . emacs). So a link to a
      ;; file that is not there silently becomes an empty buffer visiting it,
      ;; and `org-open-non-existing-files' never gets a say. Give it one.
      ;; Set that variable non-nil to go back to following a link in order to
      ;; create the file.
      (when (and (not org-open-non-existing-files)
                 (night/path-checkable-p expanded-path)
                 (not (file-exists-p expanded-path)))
        (user-error "No such file: %s" expanded-path))
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

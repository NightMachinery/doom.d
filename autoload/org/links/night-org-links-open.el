;;; autoload/org/links/night-org-links-open.el -*- lexical-binding: t; -*-
;;;
(after! (org ol)
;;;
  (defun night/org-link-open-follow (path arg)
    ;; (message "path: %s, arg: %s" path arg)
;;;
    (let* ((expanded-path
            (expand-file-name path)))
      (z awaysh open (identity expanded-path))))

  (org-link-set-parameters "open" :follow #'night/org-link-open-follow)
;;;
  (defun night/org-link-zopen-follow (path arg)
    (let* (
           ;; (expanded-path
           ;;  (expand-file-name path))
           ;;  `expand-file-name' expands zshfile links like =~base/...= incorrectly, as it things they are relative to the current dir.
           )
      ;; (message "path: %s, arg: %s" path arg)
      (z awaysh zopen (identity
                       path
                       ;; expanded-path
                       ))))

  (org-link-set-parameters "zopen" :follow #'night/org-link-zopen-follow)
  (org-link-set-parameters "videofile" :follow #'night/org-link-zopen-follow)
;;;
  (provide 'night-org-links-open))

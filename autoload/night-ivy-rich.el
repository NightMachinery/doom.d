;;; autoload/night-ivy-rich.el -*- lexical-binding: t; -*-

(after! (night-ivy ivy ivy-rich)
;;;
  (setq ivy-rich-path-style
        ;; 'abbrev
        'relative)
;;;
  (defun night/ivy-rich-bookmark-info (candidate)
    (let ((filename (ivy-rich-bookmark-filename candidate)))
      (cond (filename
             (cond ((null filename)
                    "")
                   ((file-remote-p filename)
                    candidate)
                   ((file-exists-p filename)
                    (
                     night/path-abbrev-memoized
                     ;; abbreviate-file-name
                     (file-truename filename)))
                   (t filename))))))
;;;
  ;; You can see the default transformers defined in:
  ;; [[doom:.local/straight/repos/ivy-rich/ivy-rich.el::(defcustom ivy-rich-display-transformers-list]]

  (nerd-icons-ivy-rich-mode -1)
  ;; We can manually add the nerd icons whereever we want. They usually just clutter everything.

  (plist-put ivy-rich-display-transformers-list
             'counsel-company
             '(:columns (
                         (ivy-rich-candidate (:width 0.4))
                         (night/ivy-docstring (:face font-lock-doc-face)))))

  (plist-put ivy-rich-display-transformers-list
             'counsel-find-file
             '(:columns
               ((ivy-read-file-transformer)
                (ivy-rich-counsel-find-file-truename (:face font-lock-doc-face)))))

  (plist-put ivy-rich-display-transformers-list
             'counsel-M-x
             '(:columns
               ((counsel-M-x-transformer (:width 0.6))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-M-x
             '(:columns
               ((counsel-M-x-transformer (:width 0.6))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))
  (plist-put ivy-rich-display-transformers-list
             'counsel-describe-variable
             '(:columns
               ((counsel-describe-variable-transformer (:width 0.6))
                (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face)))))

  (plist-put ivy-rich-display-transformers-list
             'counsel-describe-function
             '(:columns
               ((counsel-describe-function-transformer (:width 0.6))
                (ivy-rich-counsel-function-docstring (:face font-lock-doc-face)))))

  (plist-put ivy-rich-display-transformers-list
             'counsel-projectile-find-file
             '(:columns
               ((nerd-icons-ivy-rich-file-icon)
                (counsel-projectile-find-file-transformer
                 (:width 1.0))
                ;; (nerd-icons-ivy-rich-project-file-id
                ;;  (:width 15 :face nerd-icons-ivy-rich-file-owner-face :align right))
                ;; (nerd-icons-ivy-rich-project-file-modes
                ;;  (:width 12))
                ;; (nerd-icons-ivy-rich-project-file-size
                ;;  (:width 7 :face nerd-icons-ivy-rich-size-face))
                ;; (nerd-icons-ivy-rich-project-file-modification-time
                ;;  (:face nerd-icons-ivy-rich-time-face))
                )
               :delimiter " "))

  (plist-put ivy-rich-display-transformers-list
             'counsel-bookmark
             '(:columns ((ivy-rich-candidate (:width 0.5))
                         (ivy-rich-bookmark-type)
                         (night/ivy-rich-bookmark-info))))

  (plist-put ivy-rich-display-transformers-list
             'counsel-recentf
             '(:columns
               ((
                 ;; night/path-abbrev-memoized
                 ;; even the memoized version is a bit slow

                 abbreviate-file-name

                 ;; ivy-rich-candidate

                 (:width 1.0)))))

  (plist-put ivy-rich-display-transformers-list
             'ivy-switch-buffer
             '(:columns
               ((ivy-switch-buffer-transformer (:width 0.5))
                ;; (ivy-rich-switch-buffer-size (:width 7))
                (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                ;; (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                (ivy-rich-switch-buffer-project (:width 0.18 :face success))
                (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
               :predicate
               (lambda (cand) (get-buffer cand))))

  ;; Disable =ivy-rich= for these:
  (map-delete ivy-rich-display-transformers-list 'counsel-buffer-or-recentf)
  (map-delete ivy-rich-display-transformers-list 'counsel-fzf)

  (ivy-rich-mode -1)
  (ivy-rich-mode)
  ;; Restart =ivy-rich-mode= to make the new =ivy-rich-display-transformers-list= take effect.
;;;
  )

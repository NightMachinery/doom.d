;;; autoload/night-bookmarks.el -*- lexical-binding: t; -*-
(after! (bookmark)
  (let
      ((f (concat (getenv "nightNotes") "/private/configs/" (z hostname) "/bookmarks.el")))
    (when (f-exists-p f)
      (setq bookmark-default-file f)))
;;;
  (defun night/bookmark-reload ()
    (interactive)
    (bookmark-load bookmark-default-file t))
;;;
  (setq bookmark-save-flag 1)
  (comment                              ;; @deprecated way of doing the above
   (defun night/h-bookmark-save-for-advice (&rest args)
     (bookmark-save))

   ;; (night/unadvice #'bookmark-set)
   (advice-add #'bookmark-set :after #'night/h-bookmark-save-for-advice)
   (advice-add #'bookmark-delete :after #'night/h-bookmark-save-for-advice))
;;;
(defun bookmark-store (name alist no-overwrite)
  "Store the bookmark NAME with data ALIST.
If NO-OVERWRITE is non-nil and another bookmark of the same name already
exists in `bookmark-alist', record the new bookmark without throwing away the
old one."
  (bookmark-maybe-load-default-file)
  (let ((stripped-name (copy-sequence name)))
    (set-text-properties 0 (length stripped-name) nil stripped-name)
    (if (and (not no-overwrite)
             (bookmark-get-bookmark stripped-name 'noerror))
        ;; Already existing bookmark under that name and
        ;; no prefix arg means just overwrite old bookmark.
        (let ((bm (bookmark-get-bookmark stripped-name)))
          ;; First clean up if previously location was fontified.
          (when bookmark-set-fringe-mark
            (bookmark--remove-fringe-mark bm))
          ;; Modify using the new (NAME . ALIST) format.
          (setcdr bm alist))

      ;; otherwise just cons it onto the front (either the bookmark
      ;; doesn't exist already, or there is no prefix arg.  In either
      ;; case, we want the new bookmark consed onto the alist...)

;;; @monkkeyPatched
      ;; (push (cons stripped-name alist) bookmark-alist))
      (setf (cdr (last bookmark-alist))
            (list (cons stripped-name alist))))
;;;

    ;; Added by db
    (setq bookmark-current-bookmark stripped-name)
    (setq bookmark-alist-modification-count
          (1+ bookmark-alist-modification-count))
    (if (bookmark-time-to-save-p)
        (progn (bookmark-save)))

    (setq bookmark-current-bookmark stripped-name)
    (bookmark-bmenu-surreptitiously-rebuild-list)))
;;;
  )

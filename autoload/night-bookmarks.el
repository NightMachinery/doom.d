;;; autoload/night-bookmarks.el -*- lexical-binding: t; -*-
(after! (
         ;; disabled_7667
         bookmark)
;;;
  (defun night/bookmark-reload ()
    (interactive)
    (bookmark-load bookmark-default-file t))
  (defun night/bookmark-reset-file ()
    (interactive)
    (let
        ((f (concat (getenv "nightNotes") "/private/configs/" (system-name) "/bookmarks.el")))
      (when (f-exists-p f)
        (setq bookmark-default-file f)
        (night/bookmark-reload))))
  (night/bookmark-reset-file)
;;;
  (setq bookmark-save-flag 1)
;;;
  (defvar night/h-bookmark-file-only nil
    "Non-nil while creating a bookmark without a saved file location.")

  (defun night/h-bookmark-without-position (record)
    "Remove file location data from RECORD when creating a file bookmark.
Special handlers retain their data because their locations may be essential."
    (when (and night/h-bookmark-file-only
               (bookmark-prop-get record 'filename)
               (memq (bookmark-prop-get record 'handler)
                     '(nil bookmark-default-handler)))
      (dolist (property '(position front-context-string rear-context-string))
        (setcdr record (assq-delete-all property (cdr record)))))
    record)

  (defun night/h-bookmark-set (original &optional name no-overwrite with-position)
    "Create file bookmarks without a saved location by default.
Interactively, a prefix argument records the position and context instead
of keeping duplicate names.  Lisp callers retain NO-OVERWRITE semantics.
WITH-POSITION is also used by `night/bookmark-set-with-position'."
    (interactive (list nil nil current-prefix-arg))
    (let ((night/h-bookmark-file-only (not with-position))
          ;; Without a position, a fringe mark would imply a saved location.
          (bookmark-fringe-mark (and with-position bookmark-fringe-mark)))
      (funcall original name no-overwrite)))

  (defun night/bookmark-set-with-position ()
    "Set a bookmark that records the current position and context.
This is equivalent to calling `bookmark-set' interactively with a prefix."
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'bookmark-set)))

  (advice-add #'bookmark-make-record :filter-return
              #'night/h-bookmark-without-position)
  (advice-add #'bookmark-set :around #'night/h-bookmark-set)
  (comment ;; @deprecated way of doing the above
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
        (cond
         ((last bookmark-alist) (setf (cdr (last bookmark-alist))
                                      (list (cons stripped-name alist))))
         (t (push (cons stripped-name alist) bookmark-alist))))
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

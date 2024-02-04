;;; autoload/night-recentf.el -*- lexical-binding: t; -*-
;;;
(after! (recentf night-macros)
  (defun night/h-recentf-merge (old-list new-list)
    "Return a new list combining `new-list' and `old-list' with the specified order."
    (let* ((-compare-fn nil)
           (new-exclusive (-difference new-list old-list))

           (old-exclusive (-difference old-list new-list))
           (shared-items
            (-intersection new-list old-list)
            ;; The order will be from the first argument to `-intersection'.
            ))
      (append new-exclusive old-exclusive shared-items)))

  (defun night/h-recentf-save-around (orig-fn)
    "Around advice for `recentf-save-list' to make it merge the current list with the one on disk from other emacs instances."
    (let ((instance-list (copy-list recentf-list)))
      (recentf-load-list)
      (setq recentf-list
            (night/h-recentf-merge recentf-list instance-list))
      (recentf-cleanup)
      (funcall orig-fn)))

  (advice-add 'recentf-save-list :around #'night/h-recentf-save-around)
;;;
  (setq night/recentf-auto-save-timer
        (run-with-idle-timer
         (* 1 60) ;; after being idle for exactly X mins
         t
         (lambda ()
           (night/with-messages-suppressed
             (recentf-save-list)))))
  (comment
   (cancel-timer night/recentf-auto-save-timer))
  ;; [[https://github.com/doomemacs/doomemacs/issues/3487][{REQUEST} Continuous session autosave · Issue #3487 · doomemacs/doomemacs]]
  ;; [[https://www.emacswiki.org/emacs/RecentFiles][EmacsWiki: Recent Files]]
  )

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
  (defun night/h-bookmark-save-for-advice (&rest args)
    (bookmark-save))

  ;; (night/unadvice #'bookmark-set)
  (advice-add #'bookmark-set :after #'night/h-bookmark-save-for-advice)
  (advice-add #'bookmark-delete :after #'night/h-bookmark-save-for-advice)
;;;
  )

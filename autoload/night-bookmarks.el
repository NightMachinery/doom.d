;;; autoload/night-bookmarks.el -*- lexical-binding: t; -*-

(after! (bookmark)
  (let
      ((f (concat (getenv "nightNotes") "/private/configs/" (z hostname) "/bookmarks.el")))
    (when (f-exists-p f)
      (setq bookmark-default-file f))))

;;;
;; (bookmark-load bookmark-default-file)

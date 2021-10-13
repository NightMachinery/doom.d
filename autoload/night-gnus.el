;;; autoload/night-gnus.el -*- lexical-binding: t; -*-

;; (require 'gnus) ;; @uncomment me if you actually use =gnus=
(after! (gnus)
  (setq gnus-select-method '(nntp "nntp.aioe.org"))
  (setq gnus-secondary-select-methods nil)
  (add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org"))
  )

;;; night-fuzzy.el ---                               -*- lexical-binding: t; -*-
(comment
 (require 'fuz)
 (unless (require 'fuz-core nil t)
   (fuz-build-and-load-dymod))
;;;
 (setq snails-use-exec-path-from-shell nil)
 (require 'snails)                      ;; @fatal https://github.com/manateelazycat/snails/issues/73
 (setq snails-show-with-frame nil)
;;;
 )

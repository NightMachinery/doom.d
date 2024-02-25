;;; night-truly-last.el -*- lexical-binding: t; -*-
;;;
(after! yasnippet (yas-reload-all))
;; repeated from night-last

(after! smartparens (smartparens-global-mode -1))
;;;
(require 'org)
(after! org
  ;; Something aside from [doom:modules/lang/org/config.el] is overriding this, so we need to run the setup here, as well.
  (night/org-ui-todo-setup))
;;;
(progn
  (when (night/server-alt1-p)
    (require 'circe)
    (require 'doom-modeline-core)
    (require 'night-irc)

    ;; @warn calling these before `window-setup-hook' caused emacs to hang
    (call-interactively #'=irc)
    ;; (circe "irc.zii.lilf.ir") ;; also works
    ))
;;;

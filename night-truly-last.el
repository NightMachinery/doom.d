;;; night-truly-last.el -*- lexical-binding: t; -*-
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

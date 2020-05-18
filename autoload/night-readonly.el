;;; ~/doom.d/autoload/night-readonly.el -*- lexical-binding: t; -*-

  (require 'view)
  (setq view-read-only t)               ; enter view-mode for read-only files

;;;
  (define-key view-mode-map "x" 'night/kill-window)

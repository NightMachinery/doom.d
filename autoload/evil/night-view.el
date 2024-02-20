;;; autoload/evil/night-view.el -*- lexical-binding: t; -*-

(require 'view)

(after! (view)
  (setq view-read-only t)               ; enter view-mode for read-only files

  (map! :map view-mode-map
        :n "0" #'evil-beginning-of-line-or-visual-line
        :n "x" #'night/kill-window
        )
)

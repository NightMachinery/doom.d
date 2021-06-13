;;; ~/doom.d/autoload/night-window.el -*- lexical-binding: t; -*-

(defun night/kill-window ()
  (interactive)
  (quit-window "KILL"))
;;;
(defun night/set-x-to-kill ()
  (local-set-key "x" 'night/kill-window))

;; (add-hook 'help-mode-hook 'night/set-x-to-kill)
;; (add-hook 'dired-mode-hook 'night/set-x-to-kill)
;;;

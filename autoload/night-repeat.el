;;; autoload/night-repeat.el -*- lexical-binding: t; -*-

(defun night/repeat-command ()
  (interactive)
  (let ((lc (if (eq last-command 'night/repeat-command)
                (if (boundp 'night-last-command)
                    night-last-command
                  nil)
              (progn
                (message "last-command: %s" last-command)
                last-command))))
    (setq night-last-command lc)
    (when lc (call-interactively lc))))

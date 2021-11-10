;;; autoload/night-shell-repl.el -*- lexical-binding: t; -*-

(after! (shell)
  ;; company-mode makes the shell way too slow (especially remote shells)
  (add-hook 'shell-mode-hook #'night/disable-company))

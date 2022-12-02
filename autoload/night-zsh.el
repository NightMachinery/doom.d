;;; autoload/night-zsh.el -*- lexical-binding: t; -*-

(defun night/zsh-mode-startup ()
  (interactive)
  (outline-minor-mode)
  (setq-local outline-regexp "##")

  (set-company-backend! 'sh-mode '(company-dabbrev-code company-files)))

(add-hook 'sh-mode-hook #'night/zsh-mode-startup)

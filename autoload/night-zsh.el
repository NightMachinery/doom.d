;;; autoload/night-zsh.el -*- lexical-binding: t; -*-

(defun night/zsh-mode-startup ()
  (interactive)
  (outline-minor-mode)
  (setq-local outline-regexp "##")

  ;;;
  ;; (setq-local hl-todo-keyword-faces hl-todo-keyword-faces)
  ;; (push
  ;;  `(
  ;;    ;; night/at-tag-regex
  ;;    ,(concat "@" night/at-tag-char-regex "+")
  ;;    ;; ,(concat "@" night/at-tag-main-char-regex "+")
  ;;    at-tag-face)
  ;;  hl-todo-keyword-faces)
  ;;;

  (set-company-backend! 'sh-mode '(company-dabbrev-code company-files)))

(add-hook 'sh-mode-hook #'night/zsh-mode-startup)

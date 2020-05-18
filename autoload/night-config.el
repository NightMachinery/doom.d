;;; ~/doom.d/autoload/night-config.el -*- lexical-binding: t; -*-

(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(setq-default adaptive-wrap-extra-indent 2)
(setq pop-up-frames nil) ;;Default is true.
(setq confirm-kill-emacs nil)
(+global-word-wrap-mode 't) ;; does (global-visual-line-mode 't) itself
;; (toggle-save-place-globally)

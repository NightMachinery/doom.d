;;; night-last.el -*- lexical-binding: t; -*-

;;;
;; (add-hook 'after-init-hook #'night/load-tramp-theme)
;; (add-hook 'doom-init-ui-hook #'night/load-tramp-theme)
;; (add-hook 'doom-customize-theme-hook #'night/load-tramp-theme)

(add-hook 'doom-load-theme-hook #'night/load-tramp-theme) ; works ^_^

;; (night/load-tramp-theme) ; doesn't work
;;;
(setq server-socket-dir (concat (getenv "HOME") "/tmp/.emacs-servers"))
;; see also `server-name`
;;;

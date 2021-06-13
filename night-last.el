;;; night-last.el -*- lexical-binding: t; -*-

(after! yasnippet (yas-reload-all))
;;;
;; (add-hook 'after-init-hook #'night/load-tramp-theme)
;; (add-hook 'doom-init-ui-hook #'night/load-tramp-theme)
;; (add-hook 'doom-customize-theme-hook #'night/load-tramp-theme)

(add-hook 'doom-load-theme-hook #'night/load-tramp-theme) ; works ^_^

;; (night/load-tramp-theme) ; doesn't work
;;;
(defvar night-loaded t "Use night/loaded-p to check this. Shows if our whole config has loaded completely.")

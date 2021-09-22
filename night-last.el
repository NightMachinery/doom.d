;;; night-last.el -*- lexical-binding: t; -*-

(after! yasnippet (yas-reload-all))
;;;
;; (add-hook 'after-init-hook #'night/load-tramp-theme)
;; (add-hook 'doom-init-ui-hook #'night/load-tramp-theme)
;; (add-hook 'doom-customize-theme-hook #'night/load-tramp-theme)

(add-hook 'doom-load-theme-hook #'night/load-tramp-theme) ; works ^_^

(defun night/terminfo-set ()
  (interactive)
  ;; https://emacs.stackexchange.com/questions/38597/why-does-emacs-getenv-term-return-incorrect-data
  (when (equalp (getenv "TERM")
                      "dumb"
                      )
    ;; (not (equalp (getenv "TERM")
    ;;                  (ignore-errors (z eval "echo $TERM"))
    ;;                  ))

      ;; (setenv "TERM" "xterm-emacs")
    (setenv "TERM" (getenv-internal "TERM" initial-environment))
    ))

(add-hook 'doom-load-theme-hook #'night/terminfo-set)

;; (night/load-tramp-theme) ; doesn't work
;;;
(defvar night-loaded t "Use night/loaded-p to check this. Shows if our whole config has loaded completely.")

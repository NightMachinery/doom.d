;;; autoload/night-eldoc-box.el -*- lexical-binding: t; -*-

(when (display-graphic-p)
  (require 'eldoc-box)
;;;
  (custom-set-faces! '(eldoc-box-body :background "light steel blue" :foreground "black"))
;;;
  (defun night/eldoc-box-buffer-startup ()
    (interactive)
    (toggle-truncate-lines -1))

  (add-hook! 'eldoc-box-buffer-hook #'night/eldoc-box-buffer-startup)
;;;
  (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-mode t) ;; this mode shows the tooltip at a corner, which is better
  ;; (add-hook 'eglot--managed-mode-hook #'eldoc-box-hover-at-point-mode t) ;; this mode shown the tooltip at point, which hides useful code
;;;
  )

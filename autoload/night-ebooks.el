;;; ~/doom.d/autoload/night-ebooks.el -*- lexical-binding: t; -*-

;; I couldn't get the font size right without messing up the text layout. The current solution is to set nov-text-width to achieve the desired size, then do C-- to zoom out to get the layout right.

; supports images on GUI frames. Use `t` to see the outline.
;;;
(defun my-nov-font-setup ()
  (interactive)
  (night/disable-line-numbers)
  ;; (text-scale-mode 1)
  ;; (setq text-scale-mode-amount 4)
  (setq nov-text-width 400)
  ;; (setq visual-fill-column-center-text t)
  ;; (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
  ;;                          :height 2.0)
  )
;; (add-hook 'nov-mode-hook 'visual-line-mode)
;; (add-hook 'nov-mode-hook 'visual-fill-column-mode)
(add-hook 'nov-mode-hook 'my-nov-font-setup)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

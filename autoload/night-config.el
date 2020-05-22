;;; ~/doom.d/autoload/night-config.el -*- lexical-binding: t; -*-

(comment
 ;; not needed
 (defun on-after-init ()
   (unless (display-graphic-p (selected-frame))
     (progn

       (set-face-background 'default "unspecified-bg" (selected-frame)))))

 (add-hook 'window-setup-hook 'on-after-init)
 )


(add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(setq-default adaptive-wrap-extra-indent 4)
(setq pop-up-frames nil) ;;Default is true.
(setq confirm-kill-emacs nil)
(+global-word-wrap-mode 't) ;; does (global-visual-line-mode 't) itself
(global-auto-revert-mode)
(global-display-line-numbers-mode)
(save-place-mode 1)

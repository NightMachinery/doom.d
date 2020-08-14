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

;;;
(defun night/generic-hook-fn ()
  (interactive)
  ;; (flyspell-mode-off) ; managed at init.el modules
  ;; (add-to-list 'company-backends 'company-dabbrev-code nil) ; they add other backends in front of it which makes this useless
  (night/highlight-atsign) ; @bug This hook might get called multiple times on a single buffer, and that can cause pollution.
  )
(add-hook 'after-change-major-mode-hook #'night/generic-hook-fn)
;;;

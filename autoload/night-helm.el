;;; autoload/night-helm.el -*- lexical-binding: t; -*-

(after! (helm)
  (setq helm-autoresize-max-height 60)
  (helm-autoresize-mode 1)
  ;;;
  (define-key helm-map (kbd "M-<up>") #'helm-previous-page)
  (define-key helm-map (kbd "M-<down>") #'helm-next-page)
;;;
  ;; @todo2/wait I can't find what command scrolls the helm window itself. I tried `(define-key helm-map (kbd "S-<up>") #'scroll-down-command)`, but this tries to scroll the minibuffer, not the helm window.
  ;;
  ;; (define-key helm-map (kbd "S-<up>") #'scroll-up-command)
  ;; (define-key helm-map (kbd "S-<down>") #'night/scroll-halfpage-up)
  )

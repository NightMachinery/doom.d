;;; ~/doom.d/autoload/night-config.el -*- lexical-binding: t; -*-

(comment
 ;; not needed
 (defun on-after-init ()
   (unless (display-graphic-p (selected-frame))
     (progn
       (set-face-background 'default "unspecified-bg" (selected-frame)))))

 (add-hook 'window-setup-hook 'on-after-init)
 )
;;
(setq pop-up-frames nil) ;;Default is true.
(setq confirm-kill-emacs nil)
;;;
(global-auto-revert-mode)
(global-display-line-numbers-mode)
(save-place-mode 1)
(setq large-file-warning-threshold 50000000)
(setq tab-always-indent t)

;;;
(defun night/generic-hook-fn ()
  (interactive)
  ;; (flyspell-mode-off) ; managed at init.el modules
  ;; (add-to-list 'company-backends 'company-dabbrev-code nil) ; they add other backends in front of it which makes this useless
  ;; (night/highlight-atsign) ; bug: This hook might get called multiple times on a single buffer, and that can cause pollution.
  (hl-todo-mode)
  ;; org-babel breaks our custom TAB :(
  ;; (map!
  ;;        :map company-active-map

  ;;        ;; Make TAB always complete the current selection, instead of
  ;;        ;; only completing a common prefix.
  ;;        "<tab>"  #'company-complete-selection
  ;;        "TAB"  #'company-complete-selection)
  )
(add-hook 'after-change-major-mode-hook #'night/generic-hook-fn)
;;;
(setq bidi-paragraph-direction nil)
;;
(setq +word-wrap--major-mode-indent-var 'standard-indent) ;; @workaround @upstreamBug
;;

;; (add-hook 'visual-line-mode-hook #'adaptive-wrap-prefix-mode)
(setq-default adaptive-wrap-extra-indent 2)
(setq-default +word-wrap-extra-indent 2)

;; (+global-word-wrap-mode -1) ;; seems to be the same thing as adaptive-wrap-extra-indent
(+global-word-wrap-mode) ;; does (global-visual-line-mode 't) itself, but enable it anyway if you want it

(global-visual-line-mode 't)


;; this config seemed to make emacs hang:

;; (remove-hook 'text-mode-hook #'visual-line-mode)
;; (remove-hook 'text-mode-hook #'+word-wrap-mode)


;; (setq-default word-wrap nil)
;; (setq-default truncate-lines nil)       ;; use this instead of enabling visual-line-mode and then disabling word-wrap

;; (defun night/disable-word-wrap ()
;;   (interactive)
;;   (toggle-word-wrap -1)
;;   (setq truncate-lines nil)
;;   (adaptive-wrap-prefix-mode)
;;   ;; @futureCron was disabling this worth it?
;;   ;; it's good for URLs, but bad for normal words ...
;;   )

;; (add-hook 'text-mode-hook #'night/disable-word-wrap)
;; (add-hook 'prog-mode-hook #'night/disable-word-wrap)

;; ;; (add-hook 'visual-line-mode-hook #'night/disable-word-wrap) ;; caused an infinite loop?
;;;
(setq fast-but-imprecise-scrolling t)
(setq jit-lock-defer-time 0)
;;

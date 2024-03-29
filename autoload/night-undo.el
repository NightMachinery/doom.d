;;; autoload/night-undo.el -*- lexical-binding: t; -*-

(after! undo-fu
  (setq undo-fu-ignore-keyboard-quit t))
;;;
(require 'vundo)
(after! (vundo)
;;;
  (map!
   :leader
   "b u" #'vundo)

  (map! :map vundo--mode-map
        :nviog "<escape>" #'vundo-quit)
;;;
  (setq vundo--window-max-height 10)

  (set-popup-rule! "^\\*vundo.*\\*$" :height 0.5 :ttl nil) ;; doesn't work
;;;
  (comment
   (defun night/h-vundo-buf-setup (orig-fn &rest args)
     "upstream PR: https://github.com/casouri/vundo/issues/9"
     (let ((vundo-buf (apply orig-fn args)))
       (with-current-buffer vundo-buf
         (toggle-truncate-lines 1)
         ;; (night/wrap-soft-disable)
         ;; (evil-insert-state)
         )
       vundo-buf))
   (advice-add 'vundo-1 :around #'night/h-vundo-buf-setup)
   (comment
    (advice-remove 'vundo-1 #'night/h-vundo-buf-setup)))
;;;
  )
;;;

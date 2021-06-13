;;; autoload/night-buffer-switch-hooks.el -*- lexical-binding: t; -*-

(comment
 (defun night/buffer-switch-actions ()
   ;; @warn this hook is triggered a LOT, e.g., using brishz seems to trigger it
   ;; it's unusable :|
   ;;
   ;; @untested you can also use (interactive-p) function to test if something was run by user input.  So use hook like before and then add condition with (interactive-p)
   ;; Or maybe you want to test the value of this-command?
   ;;;
   (interactive)


   (when (not (and
               (boundp 'h-night/buffer-switch-actions-lock)
               h-night/buffer-switch-actions-lock))
     (setq h-night/buffer-switch-actions-lock t)

     ;; (message "mode: %s" major-mode)

     (night/irc-maybe-show-count)
     (setq h-night/buffer-switch-actions-lock nil)))


 ;; (remove-hook 'buffer-list-update-hook #'night/buffer-switch-actions)
 (add-hook 'buffer-list-update-hook #'night/buffer-switch-actions)
 )

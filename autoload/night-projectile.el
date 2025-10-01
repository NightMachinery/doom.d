;;; autoload/night-projectile.el -*- lexical-binding: t; -*-

(after! (projectile)
  (setq projectile-dirconfig-comment-prefix "#")
  ;;
  (setq projectile-enable-caching nil)
  ;; (setq projectile-indexing-method 'hybrid)
  (setq projectile-indexing-method 'alien)
;;;
  (defun night/h-advice-projectile-invalidate-cache (original-fn &rest args)
  "Advise `projectile-invalidate-cache' to not call `recentf-cleanup'."
  (cl-letf (((symbol-function 'recentf-cleanup) (lambda ())))
    (apply original-fn args)))

  (advice-add 'projectile-invalidate-cache :around #'night/h-advice-projectile-invalidate-cache)
;;;
  )

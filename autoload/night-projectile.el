;;; autoload/night-projectile.el -*- lexical-binding: t; -*-

(defun night/current-project-root (&optional directory)
  "Return the current project root for DIRECTORY, or nil when no project is active."
  (let* ((default-directory (or directory default-directory))
         (root
          (or
           (ignore-errors
             (when (fboundp 'projectile-project-root)
               (projectile-project-root)))
           (ignore-errors
             (when (fboundp 'project-current)
               (let ((project (project-current nil)))
                 (cond
                  ((and project (fboundp 'project-root))
                   (project-root project))
                  ((and project (fboundp 'project-roots))
                   (car (project-roots project))))))))))
    (when root
      (directory-file-name (expand-file-name root)))))


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

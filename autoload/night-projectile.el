;;; autoload/night-projectile.el -*- lexical-binding: t; -*-

(after! (projectile)
  (setq projectile-dirconfig-comment-prefix "#")
  ;;
  (setq projectile-enable-caching nil)
  ;; (setq projectile-indexing-method 'hybrid)
  (setq projectile-indexing-method 'alien)
  )

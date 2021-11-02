;;; autoload/org/links/night-org-links-phone.el -*- lexical-binding: t; -*-

(after! (org ol)
  (defun night/org-link-phone-follow (path arg)
    (message "night/org-link-phone-follow: not implemented yet; path: %s, arg: %s" path arg))

  (org-link-set-parameters "phone" :follow #'night/org-link-phone-follow)

  (defface night/org-link-face-phone '((t (:foreground "black"
                                          :background "ghostwhite"
                                          :weight bold))) "face for phone links")
  ;; (org-link-set-parameters "phone" :face 'night/org-link-face-phone)
  )

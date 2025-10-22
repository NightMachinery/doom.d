;;; autoload/org/links/night-org-links-phone.el -*- lexical-binding: t; -*-

(after! (org ol)
  (org-link-set-parameters "phone" :follow #'night/org-link-follow-copy)

  (defface night/org-link-face-phone
    '((((background dark)) (:foreground "white"
                            :background "#2F2F2F"
                            :weight bold))
      (((background light)) (:foreground "black"
                             :background "ghostwhite"
                             :weight bold)))
    "face for phone links")
  ;; (org-link-set-parameters "phone" :face 'night/org-link-face-phone)
  )

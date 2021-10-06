;;; autoload/org/links/night-org-links-jalali.el -*- lexical-binding: t; -*-

(after! (org ol)
  (defun night/org-link-jalali-follow (path arg)
    (message "night/org-link-jalali-follow: not implemented yet; path: %s, arg: %s" path arg))

  (org-link-set-parameters "jalali" :follow #'night/org-link-jalali-follow)

  (defface night/org-link-face-date '((t (:foreground "black"
                                          :background "ghostwhite"
                                          :weight bold))) "face for datetime links")
  (org-link-set-parameters "jalali" :face 'night/org-link-face-date))

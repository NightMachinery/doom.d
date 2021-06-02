;;; autoload/org/night-org-advice.el -*- lexical-binding: t; -*-

(after! org
  (advice-add #'org-forward-heading-same-level :after #'night/screen-center-ni)
  (advice-add #'org-backward-heading-same-level :after #'night/screen-center-ni)

  (advice-add #'org-previous-visible-heading :after #'night/screen-center-ni)
  (advice-add #'org-next-visible-heading :after #'night/screen-center-ni)

  (advice-add #'org-previous-link :after #'night/screen-center-ni)
  (advice-add #'org-next-link :after #'night/screen-center-ni)

  ;; (night/unadvice #'org-next-visible-heading)
  )

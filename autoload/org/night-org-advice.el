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

(after! (org-cliplink)
;;; these don't work:
  ;; (night/unadvice #'org-cliplink)
  ;; (advice-add #'org-cliplink :after #'night/after-link)


  ;; (night/unadvice #'org-cliplink-insert-transformed-title)
  ;; (advice-add #'org-cliplink-insert-transformed-title :after #'night/after-link)
;;;
  (defun org-cliplink-insert-transformed-title (url transformer)
    "Takes the URL, asynchronously retrieves the title and applies
a custom TRANSFORMER which transforms the url and title and insert
the required text to the current buffer."
    (org-cliplink-retrieve-title
     url
     (lambda (url title)
       (insert (funcall transformer url title))
       (night/after-link))))
  )

;;;
(defun night/fn-sans-without-advice (sym)
  ;; [[id:fadaf9d5-7486-48bb-a6cf-04d38ae58401][Call function without using advices defined on it]]
  "Get original function defined at SYM, sans advices."
  (if (advice--p (symbol-function sym))
      (advice--cd*r (symbol-function sym))
    (if (fboundp 'ad-get-orig-definition)
        (ad-get-orig-definition sym)
      sym)))
;;;
(after! org
  (advice-add #'better-jumper-jump-backward :after #'night/screen-center-ni)
  (advice-add #'better-jumper-jump-forward :after #'night/screen-center-ni)

  (advice-add #'org-forward-heading-same-level :after #'night/screen-center-ni)
  (advice-add #'org-backward-heading-same-level :after #'night/screen-center-ni)

  (advice-add #'org-previous-visible-heading :after #'night/screen-center-ni)
  (advice-add #'org-next-visible-heading :after #'night/screen-center-ni)

  (advice-add #'org-babel-previous-src-block :after #'night/screen-center-ni)
  (advice-add #'org-babel-next-src-block :after #'night/screen-center-ni)

  (advice-add #'org-previous-link :after #'night/screen-center-ni)
  (advice-add #'org-next-link :after #'night/screen-center-ni)

  (advice-add #'org-id-open :after #'night/screen-center-ni)
  (comment
   (advice-add #'org-open-at-point-global :after #'night/screen-center-ni)
   (advice-remove #'org-open-at-point-global #'night/screen-center-ni))

  ;; (night/unadvice #'org-next-visible-heading)
;;;
  (advice-add #'hlt-previous-highlight :after #'night/screen-center-ni)
  (advice-add #'hlt-next-highlight :after #'night/screen-center-ni)

  (advice-add #'evil-ex-search :after #'night/screen-center-ni)
  ;; (advice-add #'evil-ex-search-next :after #'night/screen-center-ni)
  ;; (advice-add #'evil-ex-search-previous :after #'night/screen-center-ni)
;;;
  )

(after! (org-cliplink)
;;; these don't work:
  ;; (night/unadvice #'org-cliplink)
  ;; (advice-add #'org-cliplink :after #'night/after-link)


  ;; (night/unadvice #'org-cliplink-insert-transformed-title)
  ;; (advice-add #'org-cliplink-insert-transformed-title :after #'night/after-link)
;;;
  (after! (org-cliplink)
    (setq org-cliplink-max-length 200)

    (defun night/org-cliplink-org-mode-link-transformer (url title)
      (if title
          (format "[[%s][%s]]" url (org-cliplink-elide-string
                                    (identity  ;; org-cliplink-escape-html4
                                               (org-cliplink-title-for-url url title))
                                    org-cliplink-max-length))
        (format "[[%s]]" url))))

  (defun night/org-cliplink-insert-transformed-title (url &optional transformer)
    "Takes the URL, asynchronously retrieves the title and applies
a custom TRANSFORMER which transforms the url and title and insert
the required text to the current buffer."
    (lexical-let ((transformer (or transformer #'night/org-cliplink-org-mode-link-transformer))
                  (m (point-marker)))
      (org-cliplink-retrieve-title
       url
       (lambda (url title)
         (save-excursion
           (goto-char m)
           (insert (funcall transformer url title)))
         (night/after-link)))))

  (advice-add 'org-cliplink-insert-transformed-title :override #'night/org-cliplink-insert-transformed-title)
  )
;;;
(defun night/save-some-buffers-ni (&rest dummy)
  (save-some-buffers))
;;;

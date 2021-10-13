(after! org
  (advice-add #'org-forward-heading-same-level :after #'night/screen-center-ni)
  (advice-add #'org-backward-heading-same-level :after #'night/screen-center-ni)

  (advice-add #'org-previous-visible-heading :after #'night/screen-center-ni)
  (advice-add #'org-next-visible-heading :after #'night/screen-center-ni)

  (advice-add #'org-previous-link :after #'night/screen-center-ni)
  (advice-add #'org-next-link :after #'night/screen-center-ni)

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
  (defun night/org-cliplink-insert-transformed-title (url &optional transformer)
    "Takes the URL, asynchronously retrieves the title and applies
a custom TRANSFORMER which transforms the url and title and insert
the required text to the current buffer."
    (lexical-let ((transformer (or transformer 'org-cliplink-org-mode-link-transformer))
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

(defun night/save-some-buffers-ni (&rest dummy)
  (save-some-buffers))

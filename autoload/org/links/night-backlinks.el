;;; autoload/org/links/night-backlinks.el -*- lexical-binding: t; -*-
;;;
(cl-defun night/org-backlink-search
    (&key
     (query nil query-set-p)
     (backend-fn #'night/search-notes))
  (interactive)
  (if-let (
           (query (cond
                   (query-set-p query)
                   (t (night/org-nearest-id-get))))
           (query
            (concat
             "id:"
             (night/regex-escape-ugrep query))))
      (funcall backend-fn query)
    (message "No ID found.")))
(cl-defun night/org-internal-backlink-search
    (&key
     (query nil query-set-p)
     (backend-fn #'swiper-isearch))
  (interactive)
  (if-let (
           (query (cond
                   (query-set-p query)
                   (t (night/org-nearest-id-get))))
           (query
            (concat
             "\\[#"
             (night/regex-escape-ugrep query))))
      (funcall backend-fn query)
    (message "No ID found.")))
;;;
(provide 'night-backlinks)

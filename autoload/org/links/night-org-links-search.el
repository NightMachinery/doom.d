;;; autoload/org/links/night-org-links-search.el -*- lexical-binding: t; -*-
(after! (org ol)
  (defvar night/org-link-search-backend "Google"
    "Default search backend for `night/org-search-link`. The backends are defined in `+lookup-provider-url-alist'.")

  (defun night/org-search-link-open (path arg)
    "Open a search link with PATH using the system's web browser."
    ;; @test [[id:e087dae9-cb11-4ed2-b183-e78abdaa3aba][org-mode links]]
    ;;;

    (let* ((backend (or (assoc
                         night/org-link-search-backend
                         +lookup-provider-url-alist)
                        (error "Search backend `%s` not found" night/org-link-search-backend)))
           (url-template (-last-item backend))
           (search-url
            (format url-template (url-hexify-string path))))
      (browse-url search-url)))

  (org-link-set-parameters
   "search"
   :follow #'night/org-search-link-open)
;;;
  )

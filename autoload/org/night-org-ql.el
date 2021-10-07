;;; autoload/org/night-org-ql.el -*- lexical-binding: t; -*-

(after! (org-ql)
  (defun night/helm-org-ql-directory (&optional dir)
    "Search Org files in `org-directory' with `helm-org-ql'."
    (interactive)
    (let*
        ((dir (or dir default-directory)))
      (helm-org-ql (org-ql-search-directories-files
                    :directories (list dir)
                    :recurse t
                    :regexp org-ql-search-directories-files-regexp)
                   :name (format! "Directory %S" dir))))

  (comment
   (org-ql-search (org-ql-search-directories-files
                   :directories (list (getenv "nightNotes"))
                   :recurse t)
     '(tags "great"))
   (org-ql-search (org-ql-search-directories-files
                   :directories (list (z path-unabbrev ~nt/bookmarks/useme))
                   :recurse t)
     '(heading "link")))
  )

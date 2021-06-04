;;; autoload/org/night-ql.el -*- lexical-binding: t; -*-

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

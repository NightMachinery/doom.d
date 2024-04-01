;;; autoload/org/links/night-org-links-grep.el -*- lexical-binding: t; -*-
;;;
(after! (org ol)
  (defun night/org-link-search-notes-follow (path arg)
    (let* ((backend-fn #'night/search-notes)
           (query path)
           (query
            (night/regex-escape-ugrep query)))
      (funcall backend-fn query)))

  (org-link-set-parameters
   "notes-search"
   :follow #'night/org-link-search-notes-follow)
;;;
;; * [[id:c03335c9-d71e-441d-a359-91b2a1f6a9ac][grep ID links]]

  (defun night/org-link-id-grep-follow (path arg)
    (let* ((backend-fn #'night/search-notes)
           (query path)
           (query
            (concat
             "^\\s*:ID:\\s+"
             (night/regex-escape-ugrep query))))
      (funcall backend-fn query)))
  (org-link-set-parameters
   "grep-id-v1"
   :follow #'night/org-link-id-grep-follow)
;;;
  (defvar night/org-id-grep-search-dirs
    (list
     (getenv "nightNotes")
     (expand-file-name "~/code/uni"))
    "The directories that `night/org-link-id-grep-follow-v2' searches in.")

  (defun night/org-link-id-grep-follow-v2 (path arg)
    (let ((search-dirs
           night/org-id-grep-search-dirs))
      (cl-loop
       for dir in search-dirs
       for found = nil
       do
       (let* ((query
               (concat "^\\s*:ID:\\s+" (night/regex-escape-ugrep path)))
              (ugrep-command
               (concat
                "ugrep --hidden --ignore-binary --file-extension=org --json --dereference-recursive --files-with-matches --max-count=1 --line-number "
                " --exclude-dir=**/DecompV-Notebooks/metrics/ " ; @hack to speed things up
                ;; @todo0 [[id:1489eb82-74fb-47dd-9991-e773b8525ca7][max-count across all files · Issue #378 · Genivia/ugrep]]
                (shell-quote-argument query)
                " "
                (shell-quote-argument dir)))
              (output (shell-command-to-string ugrep-command))
              (json-data (ignore-errors (json-read-from-string output))))
         ;; (kill-new ugrep-command)
         (comment
          (message "JSON data: %s\nlisp: %s\nType: %s"
                   json-data
                   (listp json-data)
                   (type-of json-data)))
         (cond
          ((and json-data (vectorp json-data) (> (length json-data) 0))
           (let* ((first-match (aref json-data 0))
                  (file (cdr (assoc 'file first-match)))
                  (matches (cdr (assoc 'matches first-match)))
                  (first-match-data (aref matches 0))
                  (line-number (cdr (assoc 'line first-match-data))))
             (cond
              ((and file (> line-number 0))
               (progn
                 (find-file file)
                 (goto-char (point-min))
                 (forward-line (1- line-number))
                 ;; (nav-flash-show)
                 (comment
                  (message
                   "Found and opened file: %s, line: %d"
                   (f-filename file) line-number)))
               (setq found t))
              (t
               (message "Found a match but failed to navigate to it. File: %s, Line: %s"
                        (or file "N/A") (or line-number "N/A"))))))
          (t
           (comment
            (message "No matches found for ID: %s in directory: %s" path dir)))))
       until found
       finally (unless found
                 (message "No matches found in any of the search directories (ID: %s)" path)))))

  (org-link-set-parameters
   "gid"
   :follow #'night/org-link-id-grep-follow-v2)
;;;
  )

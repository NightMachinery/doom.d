;;; autoload/org/links/night-org-links-grep.el -*- lexical-binding: t; -*-
;;;
(after! (org ol org-id)
;;;
  (defun night/org-id-locations-db-reset ()
    (interactive)
    ;; Somehow `org-id-locations' keeps getting corrupted such that =(hash-table-p org-id-locations)= returns false. This seems to be alwasy the case on emacs startup, but it happens again randomly, too.
    (setq org-id-locations (make-hash-table)))
  (defun night/h-maybe-org-id-locations-db-reset (&optional &rest dummy)
    (unless (hash-table-p org-id-locations)
      (night/org-id-locations-db-reset)))
  (night/org-id-locations-db-reset)
  (advice-add 'org-store-link :before #'night/h-maybe-org-id-locations-db-reset)
  ;; (org-id-update-id-locations nil t)
  ;; This loads the ID links. Without it, =(hash-table-p org-id-locations)= returned nil for me, which in turn made `org-id-store-link' not work properly.
;;;
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
     (getenv "NIGHTDIR")
     (expand-file-name "~/code/uni"))
    "The directories that `night/org-id-find-grep' searches in.")

  (defun night/org-id-find-grep (path &optional search-dirs)
    (let ((search-dirs
           night/org-id-grep-search-dirs))
      (cl-loop
       for dir in search-dirs
       for found = nil
       do
       (let* ((query
               (concat
                "^\\s*:[iI][dD]:\\s+"
                (night/regex-escape-ugrep path)
                "\\s*$"
                ))
              (ugrep-command
               (concat
                "ugrep --hidden --ignore-binary --file-extension=org --json --dereference-recursive --files-with-matches --max-count=1 --max-files=1 --line-number "
                " --exclude-dir=**/DecompV-Notebooks/metrics/ " ; @hack to speed things up
                (shell-quote-argument query)
                " "
                (shell-quote-argument dir)))
              (output (shell-command-to-string ugrep-command))
              (json-data (ignore-errors (json-read-from-string output))))
         ;; (message "ugrep-command:\n%s\nugrep output:\n%s" ugrep-command output)
         (cond
          ((and json-data (vectorp json-data) (> (length json-data) 0))
           (let* ((first-match (aref json-data 0))
                  (file (cdr (assoc 'file first-match)))
                  (matches (cdr (assoc 'matches first-match)))
                  (first-match-data (aref matches 0))
                  (line-number (cdr (assoc 'line first-match-data))))
             (when (and file
                        ;; (> line-number 0)
                        )
               (setq found (list :file file :line-number line-number)))))
          (t
           (comment
            (message "No matches found for ID: %s in directory: %s" path dir)))))
       until found
       finally return found)))

  (defun night/org-link-id-grep-follow-v2 (path arg)
    "@deprecated See `night/org-id-find' instead."
    (let ((result (night/org-id-find-grep path)))
      (if result
          (let ((file (plist-get result :file))
                (line-number (plist-get result :line-number)))
            (find-file file)
            (goto-char (point-min))
            (forward-line (1- line-number))
            ;; (nav-flash-show)
            (;; progn
             comment
             (message
              "Found and opened file: %s, line: %d"
              (f-filename file) line-number)))
        (message "No matches found in any of the search directories (ID: %s)" path))))

  (org-link-set-parameters
   "gid"
   :follow #'night/org-link-id-grep-follow-v2)
;;;
  (cl-defun night/h-org-id-find (id &optional (markerp nil) (fallback-modes nil) (default-mode nil) (store-in-db-p t))
    "Return the location of the entry with the id ID.
The return value is a cons cell (file-name . position), or nil
if there is no entry with that ID.
With optional argument MARKERP, return the position as a new marker."
    (let* (
           (default-mode
            (or default-mode
                "id-locations"))
           (id
            (cond
             ((symbolp id) (symbol-name id))
             ((numberp id) (number-to-string id))
             (t id)))
           org-agenda-new-buffers       ; set this to nil, idk why really
           (file
            (cond
             ((equal default-mode "id-locations")
              (org-id-find-id-file id))
             ((equal default-mode "update-id-locations")
              (org-id-update-id-locations nil t)
              (org-id-find-id-file id))
             ((equal default-mode "id-grep")
              (plist-get
               (night/org-id-find-grep id)
               :file))))
           where)
      (when file
        ;; (message "night/org-id-find: found file: %s" file)
        (setq where (org-id-find-id-in-file id file markerp))
        ;; If there is a problem in the property list of the target heading (e.g., it contains a commented line), `org-id-find-id-in-file` fails.
        (when (and store-in-db-p
                   where
                   (equalp default-mode "id-grep")
                   ;; only =id-grep= does not already store in the DB
                   )
          (org-id-add-location id file)))
      (if where
          where
        (when (length> fallback-modes 0)
          (let
              ((remaining-modes (cdr fallback-modes)))
            (night/h-org-id-find id markerp
                                 remaining-modes
                                 (car fallback-modes)
                                 store-in-db-p))))))
  (defun night/org-id-find (id &optional markerp fallback-modes default-mode)
    ;; (message "night/org-id-find: id: %s" id)
    (night/h-org-id-find
     id markerp
     (or fallback-modes
         (list
          "id-grep"
          ;; "update-id-locations"
          ))
     default-mode))
  (advice-add 'org-id-find :override #'night/org-id-find)
;;;
  (advice-remove 'org-id-open #'+org--follow-search-string-a)
  (advice-remove 'org-roam-id-open #'+org--follow-search-string-a)

  (defadvice! night/h-org-id-open-extras (fn link &optional arg)
    ;; This is forked from `+org--follow-search-string-a'.
    ;; I don't know if this will break links for export backends or not. If it does break them, moving the functionality to `night/org-id-find' might work.
;;;
    "Support ::SEARCH syntax for id: links."
    :around #'org-id-open
    :around #'org-roam-id-open
    (save-match-data
      (cl-destructuring-bind (id &optional search)
          (split-string link "::")
        (message "night/org-id-find: id: %s search-str: %s" id search)
        (prog1 (funcall fn id arg)
          (cond
           ((null search))
           ((string-match-p "\\`[0-9]+\\'" search)
            ;; Move N lines after the ID (in case it's a heading), instead
            ;; of the start of the buffer.
            (forward-line (string-to-number option)))
           ((string-match "^/\\([^/]+\\)/$" search)
            ;; `org-link-search': If S is surrounded by forward slashes, it is interpreted as a regular expression.
            (let ((match (match-string 1 search)))
              ;; (save-excursion (org-link-search search))
              ;; `org-link-search' highlights matches and changes the visibility of the trees. I don't really like its visibility changes. The visibility changes sometimes break the following code, too (idk why). `save-restriction' doesn't prevent the changes. `org-link-search' also doesn't move the point, so the following code is necessary:
              (when (re-search-forward match)
                (goto-char (match-beginning 0)))))
           ((org-link-search search)))))))
;;;
  )

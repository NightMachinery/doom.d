;;; night-org-id-links.el ---                        -*- lexical-binding: t; -*-
;;;
;; Most of the logic is in [[./night-org-links-grep.el]].
;;;
(require 'subr-x)
;;;
(defvar night/org-last-stored-id-link-info nil
  "Most recent stored Org ID link context.
Stored as a plist with keys :link and :file.")

(defun night/org-id-path-get (id)
  (let*
      ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))

    (buffer-file-name (marker-buffer m))))

(defun night/org-id-line-get (id)
  (let* ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (with-current-buffer (marker-buffer m)
      (save-excursion
        (goto-char m)
        (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
(comment
 (org-id-find "9fe5919e-28d6-4a5f-a592-ae91f99fc169" 'marker)
 (night/org-id-path-get "9fe5919e-28d6-4a5f-a592-ae91f99fc169")
 (night/org-id-line-get "9fe5919e-28d6-4a5f-a592-ae91f99fc169")
 (night/org-id-path-get "61d2cdd6-23fd-4e31-a27b-1a1c61759be4")
 (night/org-id-line-get "61d2cdd6-23fd-4e31-a27b-1a1c61759be4"))
;;;
(defun night/org-id-to-parse (path)
  "Parse id-to PATH and return (:project PROJECT :payload PAYLOAD)."
  (unless (string-match "\\`\\([^:]+\\)::\\(.+\\)\\'" path)
    (user-error "Malformed id-to link: %s" path))
  (list
   :project (match-string 1 path)
   :payload (match-string 2 path)))

(defun night/org-id-to-project-root-resolve (project)
  "Resolve PROJECT for an id-to link via ~[PROJECT]/."
  (let* ((project-ref (concat "~[" project "]/"))
         (resolved-raw (night/path-unabbrev project-ref))
         (resolved (and (stringp resolved-raw)
                        (string-trim resolved-raw)))
         (root (and (stringp resolved)
                    (not (string-empty-p resolved))
                    (directory-file-name (expand-file-name resolved)))))
    (unless (and root (file-directory-p root))
      (user-error
       "Cannot resolve id-to project `%s' via %s"
       project
       project-ref))
    root))

(defun night/org-link-id-to-follow (path arg)
  "Follow id-to PATH with ARG."
  (let* ((parsed (night/org-id-to-parse path))
         (root
          (night/org-id-to-project-root-resolve
           (plist-get parsed :project)))
         (payload (plist-get parsed :payload))
         (default-directory (file-name-as-directory root)))
    (org-id-open payload arg)))

(defun night/h-org-record-last-stored-id-link (&rest _)
  "Remember the latest stored id link and its source file."
  (let* ((link (plist-get org-store-link-plist :link))
         (file (buffer-file-name (buffer-base-buffer))))
    (when (and (stringp link)
               (string-prefix-p "id:" link)
               (stringp file))
      (setq night/org-last-stored-id-link-info
            (list :link link :file file)))))

(defun night/org-stored-link-latest-get ()
  "Return the most recent entry from `org-stored-links'."
  (or (car org-stored-links)
      (user-error "No stored Org links available")))

(defun night/org-stored-link-id-payload-get (link)
  "Return the full ID payload from stored LINK."
  (unless (and (stringp link)
               (string-prefix-p "id:" link))
    (user-error "Latest stored link is not an id: link: %S" link))
  (substring link (length "id:")))

(defun night/org-last-stored-id-link-file-get (link)
  "Return the cached source file for stored id LINK."
  (let ((cached-link (plist-get night/org-last-stored-id-link-info :link))
        (cached-file (plist-get night/org-last-stored-id-link-info :file)))
    (unless (and (equal link cached-link)
                 (stringp cached-file)
                 (file-exists-p cached-file))
      (user-error
       "No cached file context for the latest stored id link; store the link again"))
    cached-file))

(defun night/paste-id-to-link ()
  "Insert an id-to link from the latest stored id link."
  (interactive)
  (let* ((entry (night/org-stored-link-latest-get))
         (stored-link (car entry))
         (desc (cadr entry))
         (payload (night/org-stored-link-id-payload-get stored-link))
         (target-file (night/org-last-stored-id-link-file-get stored-link))
         (project-root
          (night/current-project-root
           (file-name-directory target-file)))
         (project
          (and project-root
               (file-name-nondirectory
                (directory-file-name project-root)))))
    (unless (and project (not (string-empty-p project)))
      (user-error
       "Cannot derive a project root for stored ID target: %s"
       target-file))
    (insert
     (org-link-make-string
      (concat "id-to:" project "::" payload)
      desc))))

(map!
     :map org-mode-map
     :leader
     "n;" #'night/paste-id-to-link
     )
;;;
(after! (org ol org-id)
  (advice-add 'org-id-store-link :after #'night/h-org-record-last-stored-id-link)
  (org-link-set-parameters "id-to" :follow #'night/org-link-id-to-follow))
;;;
(cl-defun night/org-ensure-heading-ids (&key scope skip)
  "Add ID properties to all headings that lack both ID and CUSTOM_ID.
When called interactively, uses region if active, otherwise entire buffer.
SCOPE can be 'file (entire buffer), 'tree, or 'region.
SKIP if non-nil should be 'archive or 'comment to skip those trees."
  (interactive)
  (let ((verbosity-level 0)
        (scope (or scope
                   (if (and (region-active-p)
                            (use-region-p))
                       'region
                     'file))))
    (condition-case err
        (cl-labels ((log-message (level fmt &rest args)
                      (when (>= verbosity-level level)
                        (apply #'message fmt args)))

                    (night/h-heading-needs-id-p ()
                      "Check if current heading needs an ID."
                      (not (or (org-entry-get nil "ID")
                               (org-entry-get nil "CUSTOM_ID"))))

                    (night/h-process-heading ()
                      "Process a single heading, adding ID if needed."
                      (when (night/h-heading-needs-id-p)
                        (log-message 1 "Adding ID to heading: %s" (org-get-heading t t t t))
                        (org-id-get-create))))
          ;; Main processing logic
          (org-map-entries #'night/h-process-heading nil scope skip)
          (log-message 0 "Finished adding IDs to headings"))
      (error
       (message "Error while adding IDs: %s" (error-message-string err))))))

;; Convenience functions for different scopes
(defun night/org-ensure-heading-ids-buffer ()
  "Add IDs to all headings in the current buffer."
  (interactive)
  (night/org-ensure-heading-ids :scope 'file))

(defun night/org-ensure-heading-ids-tree ()
  "Add IDs to all headings in the current subtree."
  (interactive)
  (night/org-ensure-heading-ids :scope 'tree))

(defun night/org-ensure-heading-ids-region ()
  "Add IDs to all headings in the active region."
  (interactive)
  (if (and (region-active-p) (use-region-p))
      (night/org-ensure-heading-ids :scope 'region)
    (user-error "No active region")))
;;;
(provide 'night-org-id-links)
;;; night-org-id-links.el ends here

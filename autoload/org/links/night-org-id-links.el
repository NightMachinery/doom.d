;;; night-org-id-links.el ---                        -*- lexical-binding: t; -*-
;;;
;; Most of the logic is in [[./night-org-links-grep.el]].
;;;
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

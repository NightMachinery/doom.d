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

(defun night/h-org-id-store-link-fallback-desc ()
  "Return the ID-location-based description for a link stored at point.

Mirrors the `desc' computation in `org-id-store-link': the `#+TITLE:'
keyword (or the file name) before the first heading, otherwise the heading
text.  Must be called with point where `org-id-store-link' was called."
  (let* ((id-location (or (and org-entry-property-inherited-from
                               (marker-position org-entry-property-inherited-from))
                          (save-excursion
                            (org-back-to-heading-or-point-min t)
                            (point))))
         (case-fold-search nil)
         (desc
          (save-excursion
            (goto-char id-location)
            (cond
             ((org-before-first-heading-p)
              (let ((keywords (org-collect-keywords '("TITLE"))))
                (if keywords
                    (cadr (assoc "TITLE" keywords))
                  (file-name-nondirectory
                   (buffer-file-name (buffer-base-buffer))))))
             ((looking-at org-complex-heading-regexp)
              (if (match-end 4)
                  (match-string 4)
                (match-string 0)))
             (t nil)))))
    (when (stringp desc)
      (substring-no-properties desc))))

(defun night/h-org-id-store-link-keep-desc (orig-fn &rest args)
  "Keep the title/heading description when the precise target supplies none.

@upstreamBug `org-id-store-link' first derives a good description from the
`#+TITLE:' keyword or the heading at the ID location, then (when
`org-link-context-for-files' and `org-id-link-use-context' are on)
unconditionally overwrites it with the description of
`org-link-precise-link-target'.  That description is nil by design for
region- and current-line-based targets, e.g. for any line before the first
heading.  The result is a link like
=id:UUID::+TITLE: Foo= with no description at all."
  (let ((res (apply orig-fn args)))
    (when (and res
               (null (plist-get org-store-link-plist :description)))
      (let ((desc (night/h-org-id-store-link-fallback-desc)))
        (when (org-string-nw-p desc)
          ;; `org-link-add-props', not `org-link-store-props': the latter
          ;; replaces `org-store-link-plist' wholesale.
          (org-link-add-props :description desc))))
    res))

(defun night/h-org-link-noise-context-line-p ()
  "Non-nil when the line at point should never become a link search string.

These are metadata lines: `#+KEYWORD:' lines, comments, property drawers and
their entries, and `#+begin_'/`#+end_' block delimiters."
  (save-excursion
    (save-match-data
      (or (org-at-block-p)
          (memq (org-element-type (org-element-at-point))
                '(keyword comment property-drawer node-property))))))

(defun night/h-org-link-precise-target-skip-noise (orig-fn &rest args)
  "Don't build a context search string from metadata lines.

@upstreamBug Before the first heading, `org-link-precise-link-target' falls
back to `org-current-line-string' without checking what kind of line it is
\(only blank lines are filtered out).  But the preamble is normally all
metadata: `#+title:', comments, and the file-level property drawer holding
the very ID being linked to.  So storing a link on the `#+title:' line yields
=id:UUID::+title: Foo=, whose search string re-targets exactly where the bare
=id:UUID= already lands.

Regions and `#+name'd elements take precedence upstream and are left alone."
  (if (and (not (org-link--context-from-region))
           (derived-mode-p 'org-mode)
           (org-before-first-heading-p)
           (not (org-element-property :name (org-element-at-point)))
           (night/h-org-link-noise-context-line-p))
      nil
    (apply orig-fn args)))

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

(defun night/org-insert-id-to-project-link ()
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
     "n;" #'night/org-insert-id-to-project-link
     )
;;;
(after! (org ol org-id)
  (advice-add 'org-id-store-link :after #'night/h-org-record-last-stored-id-link)
  (advice-add 'org-id-store-link :around #'night/h-org-id-store-link-keep-desc)
  (advice-add 'org-link-precise-link-target :around
              #'night/h-org-link-precise-target-skip-noise)
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

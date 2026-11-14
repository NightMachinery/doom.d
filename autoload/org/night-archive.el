;;; autoload/org/night-archive.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(add-to-list 'auto-mode-alist '("\\.\\(org_archive\\)\\'" . org-mode))

(defcustom night/org-archive-save-mode 'auto
  "How `org-archive-subtree' saves buffers after archiving.
`auto' saves the source buffer and archive buffer without prompting.
`ask' prompts to save the source buffer and archive buffer.
`ask-all' calls `save-some-buffers' to prompt for all modified buffers.
nil leaves both buffers unsaved."
  :group 'org-archive
  :type '(choice
          (const :tag "Auto-save source and archive buffers" auto)
          (const :tag "Prompt for source and archive buffers" ask)
          (const :tag "Prompt for all modified buffers" ask-all)
          (const :tag "Do not save" nil)))

(defvar night/h-org-archive-depth 0
  "Current nesting depth of `night/h-org-archive-subtree-around'.")

(defvar night/h-org-archive-touched-buffers nil
  "Buffers modified by the active `org-archive-subtree' invocation.")
;;;
(defun night/org-archive-done-tasks ()
  ;; https://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
  ;; @alt https://emacs.stackexchange.com/a/21952/18737
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file)
  ;; change 'tree to 'file to operate over the entire file, and viceversa to operate only on current subtree
  )
;;;
;; See also [[nightNotes:cheatsheets/emacs/orgmode/archiving.org]]
;; Copied from:
;; https://github.com/Fuco1/.emacs.d/blob/500cccb4d5bbe8c2c3a1438b9c960938117e7105/files/org-defs.el
;; https://emacs.stackexchange.com/a/49084/18737
(defun org-get-local-archive-location ()
  "Get the archive location applicable at point."
  (let ((re "^[ \t]*#\\+ARCHIVE:[ \t]+\\(\\S-.*\\S-\\)[ \t]*$")
    prop)
    (save-excursion
      (save-restriction
    (widen)
    (setq prop (org-entry-get nil "ARCHIVE" 'inherit))
    (cond
     ((and prop (string-match "\\S-" prop))
      prop)
     ((or (re-search-backward re nil t)
          (re-search-forward re nil t))
      (match-string 1))
     (t org-archive-location))))))

(defun org-extract-archive-file (&optional location)
  "Extract and expand the file name from archive LOCATION.
if LOCATION is not given, the value of `org-archive-location' is used."
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (if (= (match-beginning 1) (match-end 1))
      (buffer-file-name (buffer-base-buffer))
    (expand-file-name
     (format (match-string 1 location)
         (file-name-nondirectory
          (buffer-file-name (buffer-base-buffer))))))))

(defun org-extract-archive-heading (&optional location)
  "Extract the heading from archive LOCATION.
if LOCATION is not given, the value of `org-archive-location' is used."
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (format (match-string 2 location)
              (file-name-nondirectory
               (buffer-file-name (buffer-base-buffer))))))

(defvar org-archive-subtree-save-file-p)
(defvar org-complex-heading-regexp)
(defvar org-loop-over-headlines-in-active-region)

(defun night/h-org-archive-save-buffers (buffers)
  "Save or prompt for BUFFERS according to `night/org-archive-save-mode'."
  (let ((targets (cl-delete-duplicates (delq nil buffers) :test #'eq)))
    (cond
     ((eq night/org-archive-save-mode 'auto)
      (dolist (buf targets)
        (when (and (buffer-live-p buf)
                   (buffer-modified-p buf)
                   (buffer-file-name buf))
          (with-current-buffer buf
            (save-buffer)))))
     ((eq night/org-archive-save-mode 'ask)
      (when targets
        (save-some-buffers nil (lambda () (memq (current-buffer) targets)))))
     ((eq night/org-archive-save-mode 'ask-all)
      (save-some-buffers))
     (t nil))))

(defun night/h-org-archive-subtree-around (orig-fn &rest args)
  "Preserve archive outline hierarchy and save per `night/org-archive-save-mode'."
  (let* ((outermost-p (= night/h-org-archive-depth 0))
         (night/h-org-archive-depth (1+ night/h-org-archive-depth))
         (night/h-org-archive-touched-buffers
          (cond
           (outermost-p nil)
           (t night/h-org-archive-touched-buffers)))
         (source-buffer (or (buffer-base-buffer) (current-buffer)))
         (source-file (buffer-file-name source-buffer))
         (fix-archive-p (and source-file
                             (not (car args))
                             (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (when source-file
                  (org-extract-archive-file (org-get-local-archive-location))))
         (buffer (when afile
                   (or (find-buffer-visiting afile) (find-file-noselect afile))))
         ;; Get all the parents and their tags, we will try to
         ;; recreate the same situation in the archive buffer.
         ;; TODO: make this customizable.
         (parents-and-tags
          (when fix-archive-p
            (save-excursion
              (let (parents)
                (while (org-up-heading-safe)
                  (push (list :heading (org-get-heading t t t t)
                              :tags (org-get-tags nil :local))
                        parents))
                parents)))))
    ;; Suppress `org-archive-subtree''s intermediate save so we save once after
    ;; hierarchy reconstruction, and respect `night/org-archive-save-mode'.
    (let ((org-archive-subtree-save-file-p nil))
      (apply orig-fn args))
    (push source-buffer night/h-org-archive-touched-buffers)
    (when buffer
      (push buffer night/h-org-archive-touched-buffers))
    (when (and fix-archive-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath
                          (--map
                           (replace-regexp-in-string "^/" "" it)
                           (s-slice-at "/\\sw" olpath))))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (progn
                        (org-narrow-to-subtree)
                        (org-set-tags (plist-get (car parents-and-tags) :tags)))
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading)
                    (org-set-tags (plist-get (car parents-and-tags) :tags))
                    (end-of-line)
                    (insert "\n"))
                  (pop parents-and-tags)
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text))))))
    (when outermost-p
      (night/h-org-archive-save-buffers night/h-org-archive-touched-buffers))))

;; Retire legacy `defadvice' if reloading in a long-lived Emacs session.
(when (and (fboundp 'ad-find-advice)
           (ad-find-advice 'org-archive-subtree 'around 'fix-hierarchy))
  (ad-remove-advice 'org-archive-subtree 'around 'fix-hierarchy)
  (ad-activate 'org-archive-subtree))

(advice-add 'org-archive-subtree :around #'night/h-org-archive-subtree-around)
;;;
(defun night/h-org-toggle-heading-at-tag-single (normalized-tag)
  "Toggle NORMALIZED-TAG on the current Org heading.
Returns t when added, nil when removed."
  (save-excursion
    (org-back-to-heading t)
    (unless (looking-at org-complex-heading-regexp)
      (error "Not on an Org heading"))
    (let* ((title-beg (match-beginning 4))
           (title-end (match-end 4))
           (tags-beg (match-beginning 5))
           (anchor-end (or (match-end 3) (match-end 2) (match-end 1)))
           (tag-re (concat "\\(?:\\`\\|[ \t]+\\)\\("
                           (regexp-quote normalized-tag)
                           "\\)\\(?:[ \t]+\\|\\'\\)"))
           (case-fold-search nil)
           (removed-p nil))
      (when (and title-beg title-end)
        (save-restriction
          (narrow-to-region title-beg title-end)
          (goto-char (point-min))
          (while (re-search-forward tag-re nil t)
            (setq removed-p t)
            (let* ((m-beg (match-beginning 0))
                   (m-end (match-end 0))
                   (t-beg (match-beginning 1))
                   (t-end (match-end 1))
                   (had-leading (> t-beg m-beg))
                   (had-trailing (< t-end m-end)))
              (delete-region m-beg m-end)
              (goto-char m-beg)
              (when (and had-leading had-trailing)
                (insert " "))))))
      (cond
       (removed-p
        ;; Collapse extra whitespace between anchor and `:tags:' when the
        ;; heading title becomes empty.
        (when tags-beg
          (goto-char anchor-end)
          (when (looking-at "[ \t]+:")
            (replace-match " :"))
          (org-align-tags))
        nil)
       (title-beg
        (goto-char title-beg)
        (insert normalized-tag " ")
        (when tags-beg
          (org-align-tags))
        t)
       (t
        (goto-char anchor-end)
        (cond
         (tags-beg
          (insert " " normalized-tag)
          (org-align-tags))
         (t
          (delete-horizontal-space)
          (insert " " normalized-tag)))
        t)))))

(defun night/org-toggle-heading-at-tag (tag)
  "Toggle an @-style TAG on the current Org heading.
When TAG is present anywhere in the heading title, remove all
occurrences of it.  Otherwise insert it at the beginning of the
heading title (after stars, TODO keyword, and priority)."
  (let ((normalized-tag (cond
                         ((string-prefix-p "@" tag) tag)
                         (t (concat "@" tag))))
        res)
    (setq res
          (cond
           ((and (org-region-active-p) org-loop-over-headlines-in-active-region)
            (let ((cl (cond
                       ((eq org-loop-over-headlines-in-active-region 'start-level)
                        'region-start-level)
                       (t 'region)))
                  org-loop-over-headlines-in-active-region)
              (org-map-entries
               (lambda ()
                 (night/h-org-toggle-heading-at-tag-single normalized-tag))
               org-loop-over-headlines-in-active-region
               cl (when (org-invisible-p) (org-end-of-subtree nil t))))
            'region)
           (t
            (night/h-org-toggle-heading-at-tag-single normalized-tag))))
    (when (fboundp 'night/highlight-background)
      (night/highlight-background))
    res))

(defun night/org-toggle-retired-tag ()
  "Toggle the `@retired' at-tag on the current Org heading."
  (interactive)
  (let ((res (night/org-toggle-heading-at-tag "@retired")))
    (when (called-interactively-p 'interactive)
      (cond
       ((eq res 'region)
        (message "Toggled @retired on selected headings"))
       (res
        (message "Heading tagged @retired"))
       (t
        (message "Heading untagged @retired"))))
    res))
;;;

;;;
(setq
 ;; beware large gc thresholds: https://emacs.stackexchange.com/questions/5351/optimizing-font-lock-performance?rq=1
 ;; beware small thresholds as well -_-: emacs breaks as company continuously fills up its memory and gc throws them out
 ;;
 gcmh-idle-delay 15
 ;; gcmh-high-cons-threshold (* 512 1024 1024)
 ;; gcmh-high-cons-threshold (* 256 1024 1024)
 ;; the first number will be in megabytes. doom's default was 16mb
;;;
 ;; one of these two should be nil
 ;; gcmh-verbose t
 ;; garbage-collection-messages nil
 gcmh-verbose nil
 garbage-collection-messages t
;;;
 )
;;;
(after! smartparens (smartparens-global-mode -1))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;;;
(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  ;; https://github.com/hlissner/doom-emacs/pull/4861/
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (f-filename old-path)))
      )
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (doom--update-files old-path new-path)

    (message "File moved to %S" (abbreviate-file-name new-path))))

(defun night/update-files (&rest files)
  (let ((org-files
         (-filter #'(lambda (file)
                      (equalp (f-ext file) "org")) files)))
    (when org-files
      (org-id-update-id-locations org-files t)))
  ;; (dolist (file files)
  ;;   )
  )

(advice-add #'doom--update-files :after #'night/update-files)

;;;
(after! org

  (defun night/+org/dwim-at-point (&optional arg)
    "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
    (interactive "P")
    (if (button-at (point))
        (call-interactively #'push-button)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        ;; skip over unimportant contexts
        (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
          (setq context (org-element-property :parent context)
                type (org-element-type context)))
        (pcase type
          (`headline
           (cond ((memq (bound-and-true-p org-goto-map)
                        (current-active-maps))
                  (org-goto-ret))
                 ((and (fboundp 'toc-org-insert-toc)
                       (member "TOC" (org-get-tags)))
                  (toc-org-insert-toc)
                  (message "Updating table of contents"))
                 ((string= "ARCHIVE" (car-safe (org-get-tags)))
                  (org-force-cycle-archived))
                 ((or (org-element-property :todo-type context)
                      (org-element-property :scheduled context))
                  (org-todo
                   (if (eq (org-element-property :todo-type context) 'done)
                       (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                           'todo)
                     'done))))
           ;; Update any metadata or inline previews in this subtree
           (org-update-checkbox-count)
           (org-update-parent-todo-statistics)
           (when (and (fboundp 'toc-org-insert-toc)
                      (member "TOC" (org-get-tags)))
             (toc-org-insert-toc)
             (message "Updating table of contents"))
           (let* ((beg (if (org-before-first-heading-p)
                           (line-beginning-position)
                         (save-excursion (org-back-to-heading) (point))))
                  (end (if (org-before-first-heading-p)
                           (line-end-position)
                         (save-excursion (org-end-of-subtree) (point))))
                  (overlays (ignore-errors (overlays-in beg end)))
                  (latex-overlays
                   (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                               overlays))
                  (image-overlays
                   (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                               overlays)))
             (+org--toggle-inline-images-in-subtree beg end)
             (if (or image-overlays latex-overlays)
                 (org-clear-latex-preview beg end)
               (org--latex-preview-region beg end))))

          (`clock (org-clock-update-time-maybe))

          (`footnote-reference
           (org-footnote-goto-definition (org-element-property :label context)))

          (`footnote-definition
           (org-footnote-goto-previous-reference (org-element-property :label context)))

          ((or `planning `timestamp)
           (org-follow-timestamp-link))

          ((or `table `table-row)
           (if (org-at-TBLFM-p)
               (org-table-calc-current-TBLFM)
             (ignore-errors
               (save-excursion
                 (goto-char (org-element-property :contents-begin context))
                 (org-call-with-arg 'org-table-recalculate (or arg t))))))

          (`table-cell
           (org-table-blank-field)
           (org-table-recalculate arg)
           (when (and (string-empty-p (string-trim (org-table-get-field)))
                      (bound-and-true-p evil-local-mode))
             (evil-change-state 'insert)))

          (`babel-call
           (org-babel-lob-execute-maybe))

          (`statistics-cookie
           (save-excursion (org-update-statistics-cookies arg)))

          ((or `src-block `inline-src-block)
           (org-babel-execute-src-block arg))

          ((or `latex-fragment `latex-environment)
           (org-latex-preview arg))

          (`link
           (let* ((lineage (org-element-lineage context '(link) t))
                  (path (org-element-property :path lineage)))
             (if (and window-system (or (equal (org-element-property :type lineage) "img")
                                        (and path (image-type-from-file-name path))))
                 (+org--toggle-inline-images-in-subtree
                  (org-element-property :begin lineage)
                  (org-element-property :end lineage))
               (org-open-at-point arg))))

          ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
           (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
             (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

          (_
           (if (or (org-in-regexp org-ts-regexp-both nil t)
                   (org-in-regexp org-tsr-regexp-both nil t)
                   (org-in-regexp org-link-any-re nil t))
               (call-interactively #'org-open-at-point)
             (+org--toggle-inline-images-in-subtree
              (org-element-property :begin context)
              (org-element-property :end context))))))))

  (advice-add '+org/dwim-at-point :override #'night/+org/dwim-at-point)


  (defun night/+org--insert-item (direction &optional level prefix)
    (let (
          (prefix (or prefix ""))
          (context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         ;; Position determines where org-insert-todo-heading and org-insert-item
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (org-end-of-item)
           (backward-char))

         (insert prefix)
         (org-insert-item (org-element-property :checkbox context))
         ;; Handle edge case where current item is empty and bottom of list is
         ;; flush against a new heading.
         (when (and (eq direction 'below)
                    (eq (org-element-property :contents-begin context)
                        (org-element-property :contents-end context)))
           (org-end-of-item)
           (org-end-of-line)))

        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion
                     (insert prefix)
                     (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion
                     (insert prefix)
                     (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or level (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (while (memq (preceding-char) '(?\n ?\^M))
                  ;; Go to end of line before heading
                  (forward-char -1)
                  )
                (insert prefix)
                (insert "\n" (make-string level ?*) " ")
                ))
             (`above
              (org-back-to-heading)
              (insert prefix)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1)))))
;;;
(defun night/equal-str (a b)
  ;; (message "a: %s b: %s" a b)
  (equalp (format "%s" a) (format "%s" b))
  )

(setq +file-templates-alist
      (cl-remove 'emacs-lisp-mode +file-templates-alist :test 'night/equal-str :key 'car))
;; @note you need to restart emacs for this change to take effect
;;;

;;; autoload/org/night-org-overrides.el -*- lexical-binding: t; -*-

(after! org-fold-core
  (defun org-fold-core-get-folding-spec-from-alias (spec-or-alias)
    ;; @PR/ready @regressionFix
  "Return the folding spec symbol for SPEC-OR-ALIAS.
Return nil when there is no matching folding spec."
  (when spec-or-alias
    (unless org-fold-core--spec-symbols
      (dolist (spec (org-fold-core-folding-spec-list))
        (push (cons spec spec) org-fold-core--spec-symbols)
        (dolist (alias (assq :alias (assq spec org-fold-core--specs)))
          (push (cons alias spec) org-fold-core--spec-symbols))))
    (or                                 ;; @monkeyPatched
     (alist-get spec-or-alias org-fold-core--spec-symbols)
     spec-or-alias))))
;;;
(defun night/org-return-enter (&optional indent arg interactive)
  "Goto next table row or insert a newline.

Calls `org-table-next-row' or `newline', depending on context.

When optional INDENT argument is non-nil, call
`newline-and-indent' with ARG, otherwise call `newline' with ARG
and INTERACTIVE.

When `org-return-follows-link' is non-nil and point is on
a timestamp or a link, call `org-open-at-point'.  However, it
will not happen if point is in a table or on a \"dead\"
object (e.g., within a comment).  In these case, you need to use
`org-open-at-point' directly."
  (interactive "i\nP\np")
  (let* ((context (if org-return-follows-link (org-element-context)
		    (org-element-at-point)))
         (element-type (org-element-type context)))
    (cond
     ;; In a table, call `org-table-next-row'.  However, before first
     ;; column or after last one, split the table.
     ((or (and (eq 'table element-type)
	       (not (eq 'table.el (org-element-property :type context)))
	       (>= (point) (org-element-property :contents-begin context))
	       (< (point) (org-element-property :contents-end context)))
	  (org-element-lineage context '(table-row table-cell) t))
      ;; (message "night/org-return-enter: at table")
      (if (or (looking-at-p "[ \t]*$")
	      (save-excursion (skip-chars-backward " \t") (bolp)))
	  (insert "\n")
	(org-table-justify-field-maybe)
	(call-interactively #'org-table-next-row)))
     ;; On a link or a timestamp, call `org-open-at-point' if
     ;; `org-return-follows-link' allows it.  Tolerate fuzzy
     ;; locations, e.g., in a comment, as `org-open-at-point'.
     ((and org-return-follows-link
	   (or (and (eq 'link element-type)
		    ;; Ensure point is not on the white spaces after
		    ;; the link.
		    (let ((origin (point)))
		      (org-with-point-at (org-element-property :end context)
			(skip-chars-backward " \t")
			(> (point) origin))))
	       (org-in-regexp org-ts-regexp-both nil t)
	       (org-in-regexp org-tsr-regexp-both nil  t)
	       (org-in-regexp org-link-any-re nil t)))
      ;; (message "night/org-return-enter: at link")
      (call-interactively #'org-open-at-point))
     ;; Insert newline in heading, but preserve tags.
     ((and (not (bolp))
	   (let ((case-fold-search nil))
	     (org-match-line org-complex-heading-regexp)))
      ;; (message "night/org-return-enter: at headline")
      ;; At headline.
      (cond
       ;; @monkeyPatched I disabled the complicated logic here (as it was buggy) to opt for a simple newline instead.
       (t
        (org--newline indent arg interactive))
       (t
        ;; Split line. However, if point is on keyword, priority cookie or tags, do not break any of them: add a newline after the headline instead.
        (let (
              (tags-column
               (and (match-beginning 5)
                    (save-excursion
                      (goto-char (match-beginning 5))
                      (current-column))))
              (string
               (when (and (match-end 4) (org-point-in-group (point) 4))
                 (delete-and-extract-region (point) (match-end 4)))))
          ;; Adjust tag alignment.
          (cond
           ((not (and tags-column string)))
           (org-auto-align-tags (org-align-tags))
           (t (org--align-tags-here tags-column))) ;preserve tags column
          (end-of-line)
          (org-fold-show-entry 'hide-drawers)
          (org--newline indent arg interactive)
          (when string (save-excursion (insert (org-trim string))))))))
     ;; In a list, make sure indenting keeps trailing text within.
     ((and (not (eolp))
	   (org-element-lineage context '(item)))
      ;; (message "night/org-return-enter: in a list")
      (let ((trailing-data
	     (delete-and-extract-region (point) (line-end-position))))
	(org--newline indent arg interactive)
	(save-excursion (insert trailing-data))))
     (t
      ;; (message "night/org-return-enter: else clause")
      ;; Do not auto-fill when point is in an Org property drawer.
      (let ((auto-fill-function (and (not (org-at-property-p))
				     auto-fill-function)))
	(org--newline indent arg interactive))))))
(advice-add 'org-return :override 'night/org-return-enter)

(defun night/org-dwim-at-point (&optional arg) ;; org enter at heading
  ;; @forkedFrom =+org/dwim-at-point= [[~/.emacs.d/modules/lang/org/autoload/org.el]]
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- @disabled babel-call: execute the source block
- statistics-cookie: update it.
- @disabled src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           ;; (org-element-type (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond
          ((night/audiofile-link-get-current-line :open-link-p t) t)
          ((memq (bound-and-true-p org-goto-map)
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
         ;; @monkeyPatched
         ;; (org-babel-lob-execute-maybe)
         (night/nop)
         )

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ((or `src-block `inline-src-block)
         ;; @monkeyPatched
         ;; (org-babel-execute-src-block arg)
         (night/nop)
         )

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (cond
            ((or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (progn ;; @monkeyPatched
               (message "%s"
                        (z reval-to-stdout reval-ec pbcopy-img (identity path)))
               (progn
                 (+org--toggle-inline-images-in-subtree
                  (org-element-property :begin lineage)
                  (org-element-property :end lineage)))
               (when (not (display-graphic-p))
                 (org-open-at-point arg)
                 ;; To open the image which works on Kitty.
                 )))
            (t
             ;; (message "Opening link ...")
             (org-open-at-point arg)
             ))))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))
(advice-add '+org/dwim-at-point :override 'night/org-dwim-at-point)
;;;
(defun night/org-open-at-point (&optional arg)
  "Open thing at point.
The thing can be a link, citation, timestamp, footnote, src-block or
tags.

When point is on a link, follow it.  Normally, files will be opened by
an appropriate application (see `org-file-apps').  If the optional prefix
argument ARG is non-nil, Emacs will visit the file.  With a double
prefix argument, try to open outside of Emacs, in the application the
system uses for this file type.

When point is on a timestamp, open the agenda at the day
specified.

When point is a footnote definition, move to the first reference
found.  If it is on a reference, move to the associated
definition.

When point is on a src-block of inline src-block, open its result.

When point is on a citation, follow it.

When point is on a headline, display a list of every link in the
entry, so it is possible to pick one, or all, of them.  If point
is on a tag, call `org-tags-view' instead.

On top of syntactically correct links, this function also tries
to open links and time-stamps in comments, node properties, and
keywords if point is on something looking like a timestamp or
a link."
  (interactive "P")
  (org-load-modules-maybe)
  (setq org-window-config-before-follow-link (current-window-configuration))
  (org-remove-occur-highlights nil nil t)
  (unless (run-hook-with-args-until-success 'org-open-at-point-functions)
    (let* ((context
	    ;; Only consider supported types, even if they are not the
	    ;; closest one.
	    (org-element-lineage
	     (org-element-context)
	     '(citation citation-reference clock comment comment-block
                        footnote-definition footnote-reference headline
                        inline-src-block inlinetask keyword link node-property
                        planning src-block timestamp)
	     t))
	   (type (org-element-type context))
	   (value (org-element-property :value context)))
      (cond
       ((not type) (user-error "No link found"))
       ;; No valid link at point.  For convenience, look if something
       ;; looks like a link under point in some specific places.
       ((memq type '(comment comment-block node-property keyword))
	(call-interactively #'org-open-at-point-global))
       ;; On a headline or an inlinetask, but not on a timestamp,
       ;; a link, a footnote reference or a citation.
       ((memq type '(headline inlinetask))
	(org-match-line org-complex-heading-regexp)
	(let ((tags-beg (match-beginning 5))
	      (tags-end (match-end 5)))
	  (if (and tags-beg (>= (point) tags-beg) (< (point) tags-end))
	      ;; On tags.
	      (org-tags-view
	       arg
	       (save-excursion
		 (let* ((beg-tag (or (search-backward ":" tags-beg 'at-limit) (point)))
			(end-tag (search-forward ":" tags-end nil 2)))
		   (buffer-substring (1+ beg-tag) (1- end-tag)))))
	    ;; Not on tags.
	    (pcase (org-offer-links-in-entry (current-buffer) (point) arg)
	      (`(nil . ,_)
	       (require 'org-attach)
	       (when (org-attach-dir)
		 (message "Opening attachment")
		 (if (equal arg '(4))
		     (org-attach-reveal-in-emacs)
		   (org-attach-reveal))))
	      (`(,links . ,links-end)
	       (dolist (link (if (stringp links) (list links) links))
		 (search-forward link nil links-end)
		 (goto-char (match-beginning 0))
		 (org-open-at-point arg)))))))
       ;; On a footnote reference or at definition's label.
       ((or (eq type 'footnote-reference)
	    (and (eq type 'footnote-definition)
		 (save-excursion
		   ;; Do not validate action when point is on the
		   ;; spaces right after the footnote label, in order
		   ;; to be on par with behavior on links.
		   (skip-chars-forward " \t")
		   (let ((begin
			  (org-element-property :contents-begin context)))
		     (if begin (< (point) begin)
		       (= (org-element-property :post-affiliated context)
			  (line-beginning-position)))))))
	(org-footnote-action))
       ;; On a planning line.  Check if we are really on a timestamp.
       ((and (eq type 'planning)
	     (org-in-regexp org-ts-regexp-both nil t))
	(org-follow-timestamp-link))
       ;; On a clock line, make sure point is on the timestamp
       ;; before opening it.
       ((and (eq type 'clock)
	     value
	     (>= (point) (org-element-property :begin value))
	     (<= (point) (org-element-property :end value)))
	(org-follow-timestamp-link))
       ((eq type 'src-block) (org-babel-open-src-block-result))
       ;;; @monkeyPatched
       ;; Do nothing on white spaces after an object.
       ((and
         nil
         ;; @disabled I wanted to make this work when we are on the non-existent character at the end of the lines and the last character of the line is a non-whitespace link character (e.g., the line ends with `[audiofile:x]POINT_HERE`), but now I have opted for the simple approach of just disabling this optimization.
         ;;;
         (>= (point)
             (save-excursion
               (goto-char (org-element-property :end context))
               (skip-chars-backward " \t")
               (point))))
	(user-error "Link NOT followed because we are the on whitespace after the link. See [help:night/org-open-at-point]."))
       ;;;
       ((eq type 'inline-src-block) (org-babel-open-src-block-result))
       ((eq type 'timestamp) (org-follow-timestamp-link))
       ((eq type 'link) (org-link-open context arg))
       ((memq type '(citation citation-reference)) (org-cite-follow context arg))
       (t (user-error "No link found")))))
  (run-hook-with-args 'org-follow-link-hook))

(advice-add 'org-open-at-point :override 'night/org-open-at-point)
;;;
;; [[id:c8f9c990-525e-4ead-805d-a063a9031a21][Org-mode: example blocks are no longer syntax highlighted in emacs - User Support / Org - Doom Emacs Discourse]]
(defvar night/org-fontify-block-types '("src" "example")
  "List of block types to fontify in org-mode.")

(defun night/org-fontify-block-p (block-type)
  "Check if BLOCK-TYPE is one of the types to fontify."
  (member block-type night/org-fontify-block-types))

(defun night/org-fontify-meta-lines-and-blocks-1 (limit)
  "Fontify #+ lines and blocks."
  (let ((case-fold-search t))
    (when (re-search-forward
	   (rx bol (group (zero-or-more (any " \t")) "#"
			  (group (group (or (seq "+" (one-or-more (any "a-zA-Z")) (optional ":"))
					    (any " \t")
					    eol))
				 (optional (group "_" (group (one-or-more (any "a-zA-Z"))))))
			  (zero-or-more (any " \t"))
			  (group (group (zero-or-more (not (any " \t\n"))))
				 (zero-or-more (any " \t"))
				 (group (zero-or-more any)))))
	   limit t)
      (let ((beg (match-beginning 0))
	    (end-of-beginline (match-end 0))
	    ;; Including \n at end of #+begin line will include \n
	    ;; after the end of block content.
	    (block-start (match-end 0))
	    (block-end nil)
	    (lang (match-string 7))     ; The language, if it is a source block.
	    (bol-after-beginline (line-beginning-position 2))
	    (dc1 (downcase (match-string 2)))
	    (dc3 (downcase (match-string 3)))
	    (whole-blockline org-fontify-whole-block-delimiter-line)
	    beg-of-endline end-of-endline nl-before-endline quoting block-type)
	(cond
	 ((and (match-end 4) (equal dc3 "+begin"))
	  ;; Truly a block
	  (setq block-type (downcase (match-string 5))
		;; Src, example, export, maybe more.
		quoting (member block-type org-protecting-blocks))
	  (when (re-search-forward
		 (rx-to-string `(group bol (or (seq (one-or-more "*") space)
					       (seq (zero-or-more (any " \t"))
						    "#+end"
						    ,(match-string 4)
						    word-end
						    (zero-or-more any)))))
		 ;; We look further than LIMIT on purpose.
		 nil t)
	    ;; We do have a matching #+end line.
	    (setq beg-of-endline (match-beginning 0)
		  end-of-endline (match-end 0)
		  nl-before-endline (1- (match-beginning 0)))
	    (setq block-end (match-beginning 0)) ; Include the final newline.
	    (when quoting
	      (org-remove-flyspell-overlays-in bol-after-beginline nl-before-endline)
	      (remove-text-properties beg end-of-endline
				      '(display t invisible t intangible t)))
	    (add-text-properties
	     beg end-of-endline '(font-lock-fontified t font-lock-multiline t))
	    (org-remove-flyspell-overlays-in beg bol-after-beginline)
	    (org-remove-flyspell-overlays-in nl-before-endline end-of-endline)
            (cond
	     ((and org-src-fontify-natively
                   ;; @monkeyPatched to use use `night/org-fontify-block-p'
                   (night/org-fontify-block-p block-type))
	      (save-match-data
                (org-src-font-lock-fontify-block (or lang "") block-start block-end))
	      (add-text-properties bol-after-beginline block-end '(src-block t)))
	     (quoting
	      (add-text-properties
	       bol-after-beginline beg-of-endline
	       (list 'face
		     (list :inherit
			   (let ((face-name
				  (intern (format "org-block-%s" lang))))
			     (append (and (facep face-name) (list face-name))
				     '(org-block)))))))
	     ((not org-fontify-quote-and-verse-blocks))
	     ((string= block-type "quote")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-quote t))
	     ((string= block-type "verse")
	      (add-face-text-property
	       bol-after-beginline beg-of-endline 'org-verse t)))
	    ;; Fontify the #+begin and #+end lines of the blocks
	    (add-text-properties
	     beg (if whole-blockline bol-after-beginline end-of-beginline)
	     '(face org-block-begin-line))
	    (unless (eq (char-after beg-of-endline) ?*)
	      (add-text-properties
	       beg-of-endline
	       (if whole-blockline
		   (let ((beg-of-next-line (1+ end-of-endline)))
		     (min (point-max) beg-of-next-line))
		 (min (point-max) end-of-endline))
	       '(face org-block-end-line)))
	    t))
	 ((member dc1 '("+title:" "+subtitle:" "+author:" "+email:" "+date:"))
	  (org-remove-flyspell-overlays-in
	   (match-beginning 0)
	   (if (equal "+title:" dc1) (match-end 2) (match-end 0)))
	  (add-text-properties
	   beg (match-end 3)
	   (if (member (intern (substring dc1 1 -1)) org-hidden-keywords)
	       '(font-lock-fontified t invisible t)
	     '(font-lock-fontified t face org-document-info-keyword)))
	  (add-text-properties
	   (match-beginning 6) (min (point-max) (1+ (match-end 6)))
	   (if (string-equal dc1 "+title:")
	       '(font-lock-fontified t face org-document-title)
	     '(font-lock-fontified t face org-document-info))))
	 ((string-prefix-p "+caption" dc1)
	  (org-remove-flyspell-overlays-in (match-end 2) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  ;; Handle short captions
	  (save-excursion
	    (forward-line 0)
	    (looking-at (rx (group (zero-or-more (any " \t"))
				   "#+caption"
				   (optional "[" (zero-or-more any) "]")
				   ":")
			    (zero-or-more (any " \t")))))
	  (add-text-properties (line-beginning-position) (match-end 1)
			       '(font-lock-fontified t face org-meta-line))
	  (add-text-properties (match-end 0) (line-end-position)
			       '(font-lock-fontified t face org-block))
	  t)
	 ((member dc3 '(" " ""))
	  ;; Just a comment, the plus was not there
	  (org-remove-flyspell-overlays-in beg (match-end 0))
	  (add-text-properties
	   beg (match-end 0)
	   '(font-lock-fontified t face font-lock-comment-face)))
	 (t ;; Just any other in-buffer setting, but not indented
	  (org-remove-flyspell-overlays-in (match-beginning 0) (match-end 0))
	  (remove-text-properties (match-beginning 0) (match-end 0)
				  '(display t invisible t intangible t))
	  (add-text-properties beg (match-end 0)
			       '(font-lock-fontified t face org-meta-line))
	  t))))))
(advice-add 'org-fontify-meta-lines-and-blocks-1 :override #'night/org-fontify-meta-lines-and-blocks-1)
;;;

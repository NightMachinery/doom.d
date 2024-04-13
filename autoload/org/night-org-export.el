;;; autoload/org/night-org-export.el -*- lexical-binding: t; -*-
;;;
(require 'ox-reveal)

;;;
(require 'ox-ipynb)
;;;
(after! (ob-exp)
  (setq org-export-use-babel nil))
;;;
(defun night/h-ox-ipynb-split-text (s)
  "Given a string S, split it into substrings.
Each heading is its own string. Also, split on #+ipynb-newcell and #+attr_ipynb.
Empty strings are eliminated."
  (let* ((s1 (s-slice-at org-heading-regexp s))
         ;; split headers out
         (s2 (cl-loop for string in s1
                      append
                      (if (string-match org-heading-regexp string)
                          (let ((si (split-string string "\n"))
			        heading content)
			    ;; The first one is definitely the heading. We may also
			    ;; need properties.
			    (setq heading (pop si))
			    (when (and
                                   org-export-with-properties
                                   si (s-matches? ":PROPERTIES:" (car si)))
			      ;; @monkeyPatched I added `org-export-with-properties' above.
;;;
                              (setq heading (concat "\n" heading (pop si) "\n"))
			      (while (not (s-matches? ":END:" (car si)))
			        (setq heading (concat heading (pop si) "\n")))
			      (setq heading (concat heading (pop si) "\n"))
                              )
                            (list heading
			          (mapconcat 'identity si "\n")))
                        (list string))))
         (s3 (cl-loop for string in s2
                      append
                      (split-string string "#\\+ipynb-newcell")))
	 ;; check for paragraph metadata and split on that, but keep the attribute.
	 (s4 (cl-loop for string in s3
                      append
		      ;; Note I specifically leave off the b: in this pattern so I
		      ;; can use it in the next section
                      (split-string string "^#\\+attr_ipyn")))
	 (s5 (cl-loop for string in s4 collect
		      (if (string-prefix-p "b: " string t)
		          (concat "#+attr_ipyn" string)
		        string))))

    s5))
(advice-add 'ox-ipynb-split-text :override #'night/h-ox-ipynb-split-text)
;;;
(defun night/export-org-file-to-ipynb (&optional file output-path)
  "Export the current Org file to an ipynb file.
Replaces `\[\.` with `[file:.` and `#+begin_src jupyter-python` with `#+begin_src python :eval never`.
The output file path defaults to the current file's name with the .ipynb extension.
Prompts for confirmation if the output file already exists.

It seems PNG files are exported as 'image/png' base64 in the ipynb file itself, while JPG files are exported using their paths?"
  (interactive)
  (let* ((file (or file (buffer-file-name)))
         (default-output-path (concat (file-name-sans-extension file) ".ipynb"))
         (output-path
          (or
           output-path
           (read-file-name
            "Output file: "
            ;; (f-dirname default-output-path)
            nil

            ;; default-output-path
            nil
            nil
            (file-name-nondirectory default-output-path))))
         (temp-file (make-temp-file (file-name-directory file) nil ".org"))
         (buffer-content (buffer-substring-no-properties (point-min) (point-max)))
         exported-file)
    ;; (message "default-output-path: %s" default-output-path)
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert buffer-content)
            (goto-char (point-min))
            (while (re-search-forward "\\[\\." nil t)
              (replace-match "[file:." nil t))
            (goto-char (point-min))
            (while (re-search-forward "#\\+begin_src jupyter-python" nil t)
              (replace-match "#+begin_src python :eval never" nil t))
            (write-file temp-file)
            (setq exported-file (expand-file-name (ox-ipynb-export-org-file-to-ipynb-file temp-file)))
            (kill-buffer (current-buffer)))
          (when (file-exists-p output-path)
            (unless (y-or-n-p (format "File %s already exists. Overwrite? " output-path))
              (user-error "Aborted")))
          (rename-file exported-file output-path t)
          (message "Exported to: %s" output-path)
          (z pbadd (identity output-path))
          output-path)
      (when (file-exists-p temp-file)
        (delete-file temp-file))
      (when (file-exists-p exported-file)
        (delete-file exported-file)))))
;;;
(after! ox
  (setq org-export-with-broken-links 'mark)
  (setq org-export-with-toc 1))
;;;
(defun night/org-export-string-as-utf8 (str)
  "Assume str has Org syntax, and convert it to UTF-8."
  (interactive)
  (let ((org-ascii-charset 'utf-8))
    (org-export-string-as
     str 'ascii t)))
(comment
 (night/org-export-string-as-utf8 "*wow* /hi/ you")
 ;; yes, doesn't quite work as expected
 )
;;;
(defun night/h-org-export-preprocess-add-default-setupfiles (backend)
  (message "night/h-org-export-preprocess-add-default-setupfiles: backend=%s" backend)
  (cond
   ((eq backend 'html)
    (goto-char 0)
    (insert "#+SETUPFILE: https://nightmachinery.github.io/orgmode-styles/notes_1.org\n"))
   ((eq backend 'beamer)
    (when (night/org-night-directive-present-p "night_beamer_common1")
      (goto-char 0)
      (let ((directives
             (concat
              "\n"
              ;;; moved to =pdflatex-m= instead
              ;; "#+latex_header: \\newcommand{\\globalBibPath}{"
              ;; (getenv "nightResourcesPublic")
              ;; "/latex/global_refs.bib"
              ;; "}\n"
              ;;;
              "#+latex_header: \\input{"
              (getenv "nightNotesPrivate")
              "/subjects/resume, CV/common.tex"
              "}\n"
              ;;
              "#+latex_header: \\input{"
              (getenv "nightNotesPublic")
              "/resources/beamer/night_beamer_common1.tex"
              "}\n"
              ;;
              "#+SETUPFILE: "
              (getenv "nightNotesPublic")
              "/resources/beamer/night_beamer_common1.org"
              "\n"
              ;;
              )))
        (message "night/h-org-export-preprocess-add-default-setupfiles: added directives: %s" directives)
        (insert directives)))
    (when (night/org-night-directive-present-p "night_beamer_biblio1")
      (goto-char 0)
      (let ((directives
             (concat
              "\n"
              ;;
              "#+latex_header: \\input{"
              (getenv "nightNotesPublic")
              "/resources/beamer/night_beamer_biblio1.tex"
              "}\n"
              ;;
              )))
        (message "night/h-org-export-preprocess-add-default-setupfiles: added directives: %s" directives)
        (insert directives))))))


(add-hook 'org-export-before-processing-hook #'night/h-org-export-preprocess-add-default-setupfiles)
;;;
(defun night/org-export-file-to-html (file)
  (save-current-buffer
    (let ((b (find-file-noselect file)))
      (set-buffer b)
      (let
          ((out (org-html-export-to-html)))
        (message "out: %s" out)
        (if (f-absolute-p out)
            out
          (concat default-directory out))))))
(comment
 (night/org-export-file-to-html (concat (getenv "nightNotesPublic") "/subjects/math/AI/ML/NLP/learn/courses/cs224N/gen.org")))
;;;
(setq org-html-prefer-user-labels t)

(defvar org-html--id-attr-prefix "ID-"
  "Prefix to use in ID attributes of exported HTML elements when the ID is being determined from the ID property.")

(defun night/org-html--reference (datum info &optional named-only)
  "Return an appropriate reference for DATUM.

DATUM is an element or a `target' type object.  INFO is the
current export state, as a plist.

When NAMED-ONLY is non-nil and DATUM has no NAME keyword, return
nil.  This doesn't apply to headlines, inline tasks, radio
targets and targets."
  (let* ((type (org-element-type datum))
;;;
         ;; @monkeyPatched
         (custom-id-p nil)
         (user-label
          (or
           (org-element-property
            (pcase type
              ((or `headline `inlinetask)
               (progn
                 (setq custom-id-p t)
                 :CUSTOM_ID))
              ((or `radio-target `target) :value)
              (_ :name))
            datum)
           (if-let
               ((id-property (org-element-property
                              :ID
                              datum)))
               (progn
                 (setq custom-id-p nil)
                 (concat org-html--id-attr-prefix id-property)))))
        ;;;
         )
    (cond
     ((and user-label
	   (or (plist-get info :html-prefer-user-labels)
	       ;; Used CUSTOM_ID property unconditionally.
               custom-id-p              ;; @monkeyPatched
               ))
      user-label)
     ((and named-only
	   (not (memq type '(headline inlinetask radio-target target)))
	   (not user-label))
      nil)
     (t
      (org-export-get-reference datum info)))))
(advice-add 'org-html--reference :override 'night/org-html--reference)

(comment
 (defcustom org-html-id-property-in-name-attribute-p nil
   "When nil, the \"name\" attribute in anchors will be the same as the \"id\" attribute if `org-html-allow-name-attribute-in-anchors' is non-nil. Otherwise, it won't be set at all.
When non-nil, the \"name\" attribute will be set using the ID property of the headline. This will allow crossfile ID links to work."
   :group 'org-export-html
   :version "29.0"
   :package-version '(Org . "8.0")
   :type 'boolean)

 (setq org-html-id-property-in-name-attribute-p t)

 (defun org-html-headline (headline contents info)
   "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
   (unless (org-element-property :footnote-section-p headline)
     (let* (
;;;
            ;; @monkeyPatched
            (id-anchor (org-element-property :ID headline))
            (id-attribute (if
                              (and
                               org-html-id-property-in-name-attribute-p
                               id-anchor)
                              (progn
                                (when org-html-allow-name-attribute-in-anchors
                                  (signal 'error
                                          (list "`org-html-allow-name-attribute-in-anchors' and `org-html-id-property-in-name-attribute-p' cannot be true at the same time!")))
                                (concat " name=\"ID-" id-anchor "\""))
                            ""))
;;;
            (numberedp (org-export-numbered-headline-p headline info))
            (numbers (org-export-get-headline-number headline info))
            (level (+ (org-export-get-relative-level headline info)
                      (1- (plist-get info :html-toplevel-hlevel))))
            (todo (and (plist-get info :with-todo-keywords)
                       (let ((todo (org-element-property :todo-keyword headline)))
                         (and todo (org-export-data todo info)))))
            (todo-type (and todo (org-element-property :todo-type headline)))
            (priority (and (plist-get info :with-priority)
                           (org-element-property :priority headline)))
            (text (org-export-data (org-element-property :title headline) info))
            (tags (and (plist-get info :with-tags)
                       (org-export-get-tags headline info)))
            (full-text (funcall (plist-get info :html-format-headline-function)
                                todo todo-type priority text tags info))
            (contents (or contents ""))
	    (id (org-html--reference headline info))
	    (formatted-text
	     (if (plist-get info :html-self-link-headlines)
		 (format "<a href=\"#%s\">%s</a>" id full-text)
	       full-text)))
       (if (org-export-low-level-p headline info)
           ;; This is a deep sub-tree: export it as a list item.
           (let* ((html-type (if numberedp "ol" "ul")))
	     (concat
	      (and (org-export-first-sibling-p headline info)
		   (apply #'format "<%s class=\"org-%s\">\n"
			  (make-list 2 html-type)))
	      (org-html-format-list-item
	       contents (if numberedp 'ordered 'unordered)
	       nil info nil
	       (concat (org-html--anchor id nil
                                         id-attribute ;; @monkeyPatched
                                         info) formatted-text)) "\n"
	      (and (org-export-last-sibling-p headline info)
		   (format "</%s>\n" html-type))))
	 ;; Standard headline.  Export it as a section.
         (let ((extra-class
	        (org-element-property :HTML_CONTAINER_CLASS headline))
	       (headline-class
	        (org-element-property :HTML_HEADLINE_CLASS headline))
               (first-content (car (org-element-contents headline))))
           (format "<%s %s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                   (org-html--container headline info)
                   id-attribute ;; @monkeyPatched
                   (format "outline-container-%s" id)
                   (concat (format "outline-%d" level)
                           (and extra-class " ")
                           extra-class)
                   (format "\n<h%d id=\"%s\"%s>%s</h%d>\n"
                           level
                           id
			   (if (not headline-class) ""
			     (format " class=\"%s\"" headline-class))
                           (concat
                            (and numberedp
                                 (format
                                  "<span class=\"section-number-%d\">%s</span> "
                                  level
                                  (concat (mapconcat #'number-to-string numbers ".") ".")))
                            formatted-text)
                           level)
                   ;; When there is no section, pretend there is an
                   ;; empty one to get the correct <div
                   ;; class="outline-...> which is needed by
                   ;; `org-info.js'.
                   (if (eq (org-element-type first-content) 'section) contents
                     (concat (org-html-section first-content "" info) contents))
                   (org-html--container headline info))))))))
;;;

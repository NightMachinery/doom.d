;;; autoload/org/links/night-overrides.el -*- lexical-binding: t; -*-

(after! ol
  (defun night/org-insert-link (&optional complete-file link-location description)
    "Insert a link.  At the prompt, enter the link.

Completion can be used to insert any of the link protocol prefixes in use.

The history can be used to select a link previously stored with
`org-store-link'.  When the empty string is entered (i.e. if you just
press `RET' at the prompt), the link defaults to the most recently
stored link.  As `SPC' triggers completion in the minibuffer, you need to
use `M-SPC' or `C-q SPC' to force the insertion of a space character.
Completion candidates include link descriptions.

If there is a link under cursor then edit it.

You will also be prompted for a description, and if one is given, it will
be displayed in the buffer instead of the link.

If there is already a link at point, this command will allow you to edit
link and description parts.

With a `\\[universal-argument]' prefix, prompts for a file to link to.  The \
file name can be
selected using completion.  The path to the file will be relative to the
current directory if the file is in the current directory or a subdirectory.
Otherwise, the link will be the absolute path as completed in the minibuffer
\(i.e. normally ~/path/to/file).  You can configure this behavior using the
option `org-link-file-path-type'.

With a `\\[universal-argument] \\[universal-argument]' prefix, enforce an \
absolute path even if the file is in
the current directory or below.

A `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix negates `org-link-keep-stored-after-insertion'.

If the LINK-LOCATION parameter is non-nil, this value will be used as
the link location instead of reading one interactively.

If the DESCRIPTION parameter is non-nil, this value will be used
as the default description.  If not, and the chosen link type has
a non-nil `:insert-description' parameter, that is used to
generate a description as described in `org-link-parameters'
docstring.  Otherwise, if `org-link-make-description-function' is
non-nil, this function will be called with the link target, and
the result will be the default link description.  When called
non-interactively, don't allow to edit the default description."
    (interactive "P")
    (let* ((wcf (current-window-configuration))
           (origbuf (current-buffer))
           (region (when (org-region-active-p)
                     (buffer-substring (region-beginning) (region-end))))
           (remove (and region (list (region-beginning) (region-end))))
           (desc region)
           (link link-location)
           (abbrevs org-link-abbrev-alist-local)
           (all-prefixes (append (mapcar #'car abbrevs)
                                 (mapcar #'car org-link-abbrev-alist)
                                 (org-link-types)))
           entry)
      (cond
       (link-location)                  ; specified by arg, just use it.
       ((org-in-regexp org-link-bracket-re 1)
        ;; We do have a link at point, and we are going to edit it.
        (setq remove (list (match-beginning 0) (match-end 0)))
        (setq desc (when (match-end 2) (match-string-no-properties 2)))
        (setq link (read-string "Link: "
                                (org-link-unescape
                                 (match-string-no-properties 1)))))
       ((or (org-in-regexp org-link-angle-re)
            (org-in-regexp org-link-plain-re))
        ;; Convert to bracket link
        (setq remove (list (match-beginning 0) (match-end 0))
              link (read-string "Link: "
                                (org-unbracket-string "<" ">" (match-string 0)))))
       ((member complete-file '((4) (16)))
        ;; Completing read for file names.
        (setq link (org-link-complete-file complete-file)))
       (t
        ;; Read link, with completion for stored links.
        (org-link--fontify-links-to-this-file)
        (org-switch-to-buffer-other-window "*Org Links*")
        (with-current-buffer "*Org Links*"
          (erase-buffer)
          (insert "Insert a link.
Use TAB to complete link prefixes, then RET for type-specific completion support\n")
          (when org-stored-links
            (insert "\nStored links are available with <up>/<down> or M-p/n \
\(most recent with RET):\n\n")
            (insert (mapconcat #'org-link--prettify
                               (reverse org-stored-links)
                               "\n")))
          (goto-char (point-min)))
        (when (get-buffer-window "*Org Links*" 'visible)
          (let ((cw (selected-window)))
            (select-window (get-buffer-window "*Org Links*" 'visible))
            (with-current-buffer "*Org Links*" (setq truncate-lines t))
            (unless (pos-visible-in-window-p (point-max))
              (org-fit-window-to-buffer))
            (and (window-live-p cw) (select-window cw))))
        (unwind-protect
            ;; Fake a link history, containing the stored links.
            (let ((org-link--history
                   (append (mapcar #'car org-stored-links)
                           org-link--insert-history)))
              (setq link
                    (org-completing-read
                     "Link: "
                     (append
                      (mapcar (lambda (x) (concat x ":")) all-prefixes)
                      (mapcar #'car org-stored-links)
                      ;; Allow description completion.  Avoid "nil" option
                      ;; in the case of `completing-read-default' and
                      ;; an error in `ido-completing-read' when some links
                      ;; have no description.
                      (delq nil (mapcar 'cadr org-stored-links)))
                     nil nil nil
                     'org-link--history
                     (caar org-stored-links)))
              (unless (org-string-nw-p link) (user-error "No link selected"))
              (dolist (l org-stored-links)
                (when (equal link (cadr l))
                  (setq link (car l))))
              (when (or (member link all-prefixes)
                        (and (equal ":" (substring link -1))
                             (member (substring link 0 -1) all-prefixes)
                             (setq link (substring link 0 -1))))
                (setq link (with-current-buffer origbuf
                             (org-link--try-special-completion link)))))
          (set-window-configuration wcf)
          (kill-buffer "*Org Links*"))
        (setq entry (assoc link org-stored-links))
        (or entry (push link org-link--insert-history))
        (setq desc (or desc (nth 1 entry)))))

      (when (funcall (if (equal complete-file '(64)) 'not 'identity)
                     (not org-link-keep-stored-after-insertion))
        (setq org-stored-links (delq (assoc link org-stored-links)
                                     org-stored-links)))

      (when (and (string-match org-link-plain-re link)
                 (not (string-match org-ts-regexp link)))
        ;; URL-like link, normalize the use of angular brackets.
        (setq link (org-unbracket-string "<" ">" link)))

      ;; Check if we are linking to the current file with a search
      ;; option If yes, simplify the link by using only the search
      ;; option.
      (when (and (buffer-file-name (buffer-base-buffer))
                 (let ((case-fold-search nil))
                   (string-match "\\`file:\\(.+?\\)::" link)))
        (let ((path (match-string-no-properties 1 link))
              (search (substring-no-properties link (match-end 0))))
          (save-match-data
            (when (equal (file-truename (buffer-file-name (buffer-base-buffer)))
                         (file-truename path))
              ;; We are linking to this same file, with a search option
              (setq link search)))))

      ;; Check if we can/should use a relative path.  If yes, simplify
      ;; the link.
      (let ((case-fold-search nil))
        (when (string-match "\\`\\(file\\|docview\\):" link)
          (let* ((type (match-string-no-properties 0 link))
                 (path-start (match-end 0))
                 (search (and (string-match "::\\(.*\\)\\'" link)
                              (match-string 1 link)))
                 (path
                  (if search
                      (substring-no-properties
                       link path-start (match-beginning 0))
                    (substring-no-properties link (match-end 0))))
                 (origpath path))
            (cond
             ((or (eq org-link-file-path-type 'absolute)
                  (equal complete-file '(16)))
              (setq path (abbreviate-file-name (expand-file-name path))))
             ((eq org-link-file-path-type 'noabbrev)
              (setq path (expand-file-name path)))
             ((eq org-link-file-path-type 'relative)
              (setq path (file-relative-name path)))
             ((functionp org-link-file-path-type)
              (setq path (funcall org-link-file-path-type
                                  (expand-file-name path))))
             (t
              (save-match-data
                (if (string-match (concat "^" (regexp-quote
                                               (expand-file-name
                                                (file-name-as-directory
                                                 default-directory))))
                                  (expand-file-name path))
                    ;; We are linking a file with relative path name.
                    (setq path (substring (expand-file-name path)
                                          (match-end 0)))
                  (setq path (abbreviate-file-name (expand-file-name path)))))))
            (setq link (concat type path (and search (concat "::" search))))
            (when (equal desc origpath)
              (setq desc path)))))

      (let* ((type
              (cond
               ((and all-prefixes
                     (string-match (rx-to-string `(: string-start (submatch (or ,@all-prefixes)) ":")) link))
                (match-string 1 link))
               ((file-name-absolute-p link) "file")
               ((string-match "\\`\\.\\.?/" link) "file")))
             (initial-input
              (cond
               (description)
               (desc)
               ((org-link-get-parameter type :insert-description)
                (let ((def (org-link-get-parameter type :insert-description)))
                  (condition-case nil
                      (cond
                       ((stringp def) def)
                       ((functionp def)
                        (funcall def link desc)))
                    (error
                     (message "Can't get link description from org link parameter `:insert-description': %S"
                              def)
                     (sit-for 2)
                     nil))))))
             (initial-input
              (cond
               (org-link-make-description-function
                (condition-case nil
                    (funcall org-link-make-description-function link initial-input)
                  (error
                   (message "Can't get link description from %S"
                            org-link-make-description-function)
                   (sit-for 2)
                   nil)))
               (t initial-input))))
        (setq desc (if (called-interactively-p 'any)
                       (read-string "Description: " initial-input)
                     initial-input)))

      (unless (org-string-nw-p desc) (setq desc nil))
      (when remove (apply #'delete-region remove))
      (insert (org-link-make-string link desc))
      ;; Redisplay so as the new link has proper invisible characters.
      (sit-for 0)))
  (advice-add 'org-insert-link :override 'night/org-insert-link)
  )

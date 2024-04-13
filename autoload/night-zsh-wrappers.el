;;; autoload/night-zsh-wrappers.el -*- lexical-binding: t; -*-
;;;
(setq night/zsh-timeout-default 300)    ;; in seconds
;;;
(defun night/org-link-browser-current (&optional browser)
  (interactive)
  (night/insert-for-yank-and-save
   (cond ((equal browser "edge") (z org-link-edge-current))
         ((equal browser "chrome") (z org-link-chrome-current))
         ((equal browser "arc") (z org-link-arc-current))
         (t (z org-link-browser-current))))
  (night/org-imdb-fill-maybe))

(defun night/org-link-edge-current ()
  (interactive)
  (night/org-link-browser-current "edge"))

(defun night/org-link-chrome-current ()
  (interactive)
  (night/org-link-browser-current "chrome"))

(defun night/org-link-arc-current ()
  (interactive)
  (night/org-link-browser-current "arc"))
;;;
(defun night/p-org-fanfic ()
  (interactive)
  (night/insert-for-yank-and-save
   (z p-org-fanfic))
  (night/bell-link))
;;;
(defun night/p-newline2space ()
  (interactive)
  (let ((text (z p-newline2space)))
    (comment
     (message "minibufferp: %s, evil-ex-c: %s ex-s: %s"
              (minibufferp)
              (night/in-evil-ex-completion-p)
              (night/in-evil-ex-search-p)))
    (night/insert-for-yank
     (cond
      ((and
        (minibufferp))
       ;; In minibuffer, we often want to search for this string. Using lower case allows smart-case matching.
       ;; Paper names are often capitalized differently and we do not want not to find a match for case sensitivity.
;;;
       ;; (message "bf: %s" (buffer-name))
       (night/regex-escape-smart
        (s-downcase text)))
      (t text)))))
(defalias 'night/pns #'night/p-newline2space)
;;;
(defun night/semantic-scholar-to-org-sync ()
  (interactive)
  (night/org-insert-and-fix-levels
   (z semantic-scholar-to-org))
  (night/save-buffer)
  (night/bell-link))

(night/defun-named night/semantic-scholar-to-org ()
  (interactive)
  (night/brishz-async-insert
   :name $0
   :command (list "reval-timeout" night/zsh-timeout-default "reval-paste" "semantic-scholar-to-org-emc")
   :callback-after #'night/bell-link
   :insert-fn #'night/org-insert-and-fix-levels
   :save-p t))
;;;
(night/defun-named night/url2org ()
  (interactive)
  (night/brishz-async-insert
   :name $0
   :command (list "reval-timeout" night/zsh-timeout-default "reval-paste" "url2org-emc")
   :callback-after #'night/bell-link
   :insert-fn #'night/org-insert-and-fix-levels
   :save-p t))
;;;
(night/defun-named night/paste-md2org ()
  (interactive)
  (night/brishz-async-insert
   :name $0
   :command (list "reval-paste" "md2org")
   :callback-after #'night/nop
   :insert-fn #'night/org-insert-and-fix-levels
   :save-p nil))

(night/defun-named night/paste-org2md ()
  (interactive)
  (night/brishz-async-insert
   :name $0
   :command (list "reval-paste"
                  ;; "org2md"
                  "org2md-escape-triple-quotes"
                  )
   :callback-after #'night/nop
   :insert-fn #'night/insert-for-yank
   :save-p nil))

(night/defun-named night/paste-py-escape-triple-quotes ()
  (interactive)
  (night/brishz-async-insert
   :name $0
   :command (list "reval-paste"
                  "py-escape-triple-quotes")
   :callback-after #'night/nop
   :insert-fn #'night/insert-for-yank
   :save-p nil))
;;;
(defun night/p-titlecase ()
  (interactive)
  (insert-for-yank (z reval-paste titlecase)))

(defun night/ocr ()
  (interactive)
  (insert-for-yank (z reval-paste serr ocr)))
;;;
(defun night/lilf-link-notes ()
  (interactive)
  (let ((url (z lilf-link-notes (buffer-file-name))))
    (message "%s" url)
    (kill-new url)))
;;;
(defun night/strip-prefixed-colons ()
  "If a region is selected, we copy it without its prefix colons.
If no region is currently selected, we paste the clipboard without its prefix colons."
  (interactive)
  (cond
   ((use-region-p)
    (when-let ((text (night/region-copy)))
      (kill-new (z reval-paste strip-prefixed-colons))))
   ((night/current-line-starts-with-p ": ")
    ;; When the current line starts with `: `, copy the current line
    (kill-new (night/current-line-get))
    (kill-new (z reval-paste strip-prefixed-colons)))
   (t
    (insert-for-yank (z reval-paste strip-prefixed-colons)))))
;;;
(defun night/cp-link (&rest args)
  "Copy FILES to DEST."
  (unless (and args (length> args 1))
    (error "Needs at least two arguments"))
  (let* ((dest (car (last args)))       ; Get the last argument as DEST
         (files (butlast args)))        ; Get all but the last argument as FILES
    ;; (message "Files: %s" files)
    ;; (message "Dest: %s" dest)
    (z fnswap reval-ec reval cp-link (identity files) (identity dest))))
;;;

;;; autoload/night-zsh-wrappers.el -*- lexical-binding: t; -*-
;;;
(setq night/zsh-timeout-default 300)    ;; in seconds
;;;
(defun night/org-link-browser-current ()
  (interactive)
  (night/insert-for-yank-and-save
   (z org-link-browser-current)))

(defun night/org-link-edge-current ()
  (interactive)
  (night/insert-for-yank-and-save
   (z org-link-edge-current)))
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
    (night/insert-for-yank
     (cond
      ((and
        (minibufferp))
       ;; In minibuffer, we often want to search for this this string. Using lower case allows smart-case matching.
       ;; Paper names are often capitalized differently and we do not want not to find a match for case sensitivity.
       ;;;
       ;; (message "bf: %s" (buffer-name))
       (regexp-quote
        ;; [agfi:regex-escape]
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
   :save-p t))
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
  (interactive)
  (when-let ((text (night/region-copy)))
    (kill-new (z reval-paste strip-prefixed-colons))))
;;;

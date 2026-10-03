;;; autoload/night-email.el -*- lexical-binding: t; -*-
;;; Mail: notmuch reads, msmtp sends, mbsync syncs. See docs/email.md.
;;
;; The account's identity (address, name, host) is private and lives outside
;; this repository. notmuch.el reads the name and addresses from the notmuch
;; config that email/bin/night-mail-render writes, and the account's folder is
;; discovered from the Maildir, so nothing here names the account.
;;;

(defconst night/notmuch-pinned-version "0.40"
  "The notmuch version that packages.el pins notmuch.el to.

notmuch.el must match the notmuch CLI. Keep this equal to the tag of
the pin in packages.el: `night/h-notmuch-version-check' compares it
against the CLI, and email/install-night-mail.zsh reads it from here.")

(defvar night/mail-email-dir (concat (getenv "DOOMDIR") "/email")
  "Directory with the night-mail scripts, templates and notmuch hooks.")

(defvar night/mail-sync-wait 600
  "Seconds `night/mail-sync' waits for a sync already running to finish.")

(defvar night/h-mail-sync-process nil
  "The running `night/mail-sync' process, if any.")

(defun night/mail-account ()
  "Return the mail account's folder under the notmuch mail root, or nil.

An account is a top-level folder holding an INBOX, the same rule that
email/bin/night-mail-lib.bash uses. With several, the first one wins."
  (condition-case err
      (let* ((root (car (process-lines notmuch-command "config" "get" "database.mail_root")))
             (accounts (seq-filter
                        (lambda (dir) (file-directory-p (expand-file-name "INBOX" dir)))
                        (directory-files root t "\\`[^.]"))))
        (and accounts (file-name-nondirectory (car accounts))))
    (error
     (message "night/mail-account: %s" (error-message-string err))
     nil)))

(defun night/h-notmuch-version-check ()
  "Warn if the notmuch CLI is not the version notmuch.el is pinned to."
  (let ((cli (condition-case nil
                 (car (process-lines notmuch-command "--version"))
               (error nil)))
        (want (concat "notmuch " night/notmuch-pinned-version)))
    (cond
     ((equal cli want) t)
     (t
      (display-warning
       'night-email
       (format "the notmuch CLI is %S, but notmuch.el is pinned to %s; re-pin it (see docs/email.md)"
               cli night/notmuch-pinned-version))
      nil))))

(defun night/h-mail-sync-sentinel (proc _event)
  "Report how the `night/mail-sync' process PROC ended, and refresh on success."
  (when (memq (process-status proc) '(exit signal))
    (let ((status (process-exit-status proc)))
      (cond
       ((zerop status)
        (when (fboundp 'notmuch-refresh-all-buffers)
          (notmuch-refresh-all-buffers))
        (message "night/mail-sync: done"))
       (t
        (message "night/mail-sync: failed with status %s; see the buffer %S"
                 status (buffer-name (process-buffer proc))))))))

(defun night/mail-sync ()
  "Sync mail in the background, then refresh every notmuch buffer.

Runs email/bin/night-mail-sync, which moves files by tag, runs mbsync,
indexes and tags. A sync already running (the LaunchAgent's, or another
Emacs's) is waited for, up to `night/mail-sync-wait' seconds. The
script's log goes to the buffer \" *night-mail-sync*\"."
  (interactive)
  (cond
   ((process-live-p night/h-mail-sync-process)
    (message "night/mail-sync: a sync is already running"))
   (t
    (let ((buf (get-buffer-create " *night-mail-sync*")))
      (with-current-buffer buf (erase-buffer))
      (message "night/mail-sync: syncing ...")
      (setq night/h-mail-sync-process
            (make-process
             :name "night-mail-sync"
             :buffer buf
             :command (list "/bin/bash"
                            (expand-file-name "bin/night-mail-sync" night/mail-email-dir)
                            "--wait" (number-to-string night/mail-sync-wait))
             :noquery t
             :sentinel #'night/h-mail-sync-sentinel))))))

(defun night/h-notmuch-poll-async (&rest _)
  "Run `night/mail-sync' in place of `notmuch-poll'.
`notmuch-poll' blocks Emacs for the whole sync. Advising it covers every
caller, e.g. `gR' and the hello screen's `G'."
  (night/mail-sync))
;;; HTML mail as Org
;; pandoc converts the HTML, email/html-to-org.lua strips layout tables,
;; images and raw HTML, and the result is fontified as in an Org buffer.

(defvar night/notmuch-html-renderer 'org
  "How notmuch shows a text/html part: `org' (via pandoc) or `shr'.
`night/notmuch-toggle-html-renderer' switches it.")

(defvar night/notmuch-html-org-max-size (* 4 1024 1024)
  "HTML parts larger than this many characters are left to shr.")

(defvar-local night/mail-buffer-p nil
  "Non-nil in a buffer that holds mail outside notmuch's own modes.
The LLM policy refuses such buffers like notmuch's; see
`night/h-llm-mail-read-p'.")
(put 'night/mail-buffer-p 'permanent-local t)

(defun night/h-html-to-org (html)
  "Return HTML converted to Org text by pandoc, or nil if pandoc fails."
  (let ((pandoc (executable-find "pandoc"))
        (filter (expand-file-name "html-to-org.lua" night/mail-email-dir)))
    (when pandoc
      (with-temp-buffer
        (insert html)
        (let* ((coding-system-for-read 'utf-8)
               (coding-system-for-write 'utf-8)
               (status (call-process-region
                        (point-min) (point-max) pandoc t '(t nil) nil
                        "-f" "html" "-t" "org" "--wrap=none"
                        (concat "--lua-filter=" filter))))
          (and (eql status 0) (buffer-string)))))))

(defun night/h-mail-follow-url (url)
  "Open URL if it is a web or mailto link; show any other kind instead.
Mail is untrusted, so an `elisp:' or `shell:' link must never run."
  (cond
   ((string-match-p "\\`\\(https?\\|mailto\\):" url) (browse-url url))
   (t (message "Not following this link: %s" url))))

(defun night/h-org-fontify-for-display (org)
  "Return ORG as fontified text with its bracket links turned into buttons.

Each link becomes its description, a button that opens the target via
`night/h-mail-follow-url'. Hidden emphasis markers are deleted. Faces
are copied to `font-lock-face', since font-lock in the notmuch buffer
would otherwise strip them."
  (with-temp-buffer
    (insert org)
    (let ((org-inhibit-startup t)
          (org-link-descriptive nil)
          (org-hide-emphasis-markers t))
      (delay-mode-hooks (org-mode))
      (font-lock-ensure))
    (remove-list-of-text-properties
     (point-min) (point-max) '(keymap local-map help-echo mouse-face htmlize-link))
    ;; By regex rather than org's link properties: org leaves some links
    ;; unfontified, e.g. ones whose description spans lines.
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (let* ((start (match-beginning 0))
             (url (org-link-unescape (match-string-no-properties 1)))
             (desc (if (match-beginning 2)
                       (buffer-substring (match-beginning 2) (match-end 2))
                     url)))
        (delete-region start (match-end 0))
        (goto-char start)
        (insert desc)
        (add-face-text-property start (point) 'org-link)
        (make-text-button start (point)
                          'action (lambda (_) (night/h-mail-follow-url url))
                          'follow-link t
                          'help-echo url)))
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let ((next (next-single-property-change pos 'invisible nil (point-max))))
          (if (get-text-property pos 'invisible)
              (delete-region pos next)
            (setq pos next)))))
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let ((next (next-single-property-change pos 'face nil (point-max))))
          (put-text-property pos next 'font-lock-face (get-text-property pos 'face))
          (setq pos next))))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (buffer-string)))

(defun night/h-notmuch-insert-html-as-org (msg part)
  "Insert the text/html PART of MSG as fontified Org; nil if that fails."
  (condition-case err
      (let* ((html (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))
             (org (and (<= (length html) night/notmuch-html-org-max-size)
                       (night/h-html-to-org html))))
        (when org
          (insert (night/h-org-fontify-for-display org))
          t))
    (error
     (message "night/notmuch: HTML to Org failed, using shr: %s" (error-message-string err))
     nil)))

(defun night/h-notmuch-show-html (orig msg part content-type nth depth button)
  "Show a text/html part as Org, falling back to ORIG (shr)."
  (or (and (eq night/notmuch-html-renderer 'org)
           (night/h-notmuch-insert-html-as-org msg part))
      (funcall orig msg part content-type nth depth button)))

(defun night/notmuch-toggle-html-renderer ()
  "Switch HTML mail between Org and shr, which can show images."
  (interactive)
  (setq night/notmuch-html-renderer
        (if (eq night/notmuch-html-renderer 'org) 'shr 'org))
  (when (derived-mode-p 'notmuch-show-mode)
    (notmuch-show-refresh-view))
  (message "HTML mail renders with %s" night/notmuch-html-renderer))

(defun night/h-notmuch-find-html-part (parts)
  "Return the first text/html part in the MIME tree PARTS, or nil."
  (seq-some
   (lambda (part)
     (let ((content (plist-get part :content)))
       (cond
        ((string-equal-ignore-case (or (plist-get part :content-type) "") "text/html") part)
        ;; A multipart's content is its child parts.
        ((and (consp content) (plist-get (car content) :content-type))
         (night/h-notmuch-find-html-part content)))))
   parts))

(defun night/notmuch-show-html-in-org ()
  "Open the HTML of the message at point as an Org buffer.
For what the inline view lacks: folding, `org-store-link', refiling
into notes. The buffer is read-only and marked as mail."
  (interactive)
  (let* ((msg (notmuch-show-get-message-properties))
         (part (night/h-notmuch-find-html-part (plist-get msg :body)))
         (org (and part
                   (night/h-html-to-org
                    (notmuch-get-bodypart-text msg part notmuch-show-process-crypto))))
         (oneline (lambda (s) (replace-regexp-in-string "[\n\r]+" " " (or s "")))))
    (cond
     ((not part) (user-error "This message has no HTML part"))
     ((not org) (user-error "pandoc could not convert this message"))
     (t
      ;; Read the headers here: they come from text properties of the show buffer.
      (let ((subject (funcall oneline (notmuch-show-get-subject)))
            (header (concat
                     (format "From: %s\nDate: %s\n"
                             (funcall oneline (notmuch-show-get-from))
                             (funcall oneline (notmuch-show-get-date)))
                     (format "[[notmuch:%s][Open in notmuch]]\n\n" (notmuch-show-get-message-id)))))
        (with-current-buffer (get-buffer-create (format "*mail-org: %s*" subject))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "#+title: %s\n" subject) header org))
          (org-mode)
          (setq night/mail-buffer-p t
                buffer-read-only t)
          (set-buffer-modified-p nil)
          (goto-char (point-min))
          (pop-to-buffer (current-buffer))))))))
;;;
(after! notmuch
  (setq notmuch-command (or (executable-find "notmuch") "/opt/homebrew/bin/notmuch"))
  (night/h-notmuch-version-check)

  (advice-add 'notmuch-poll :override #'night/h-notmuch-poll-async)
  (advice-add 'notmuch-show-insert-part-text/html :around #'night/h-notmuch-show-html)

  ;; Of a multipart/alternative, show the HTML (rendered as Org) rather
  ;; than the plain text. multipart/related is an HTML part with its images.
  (setq notmuch-multipart/alternative-discouraged '("text/plain"))

  (setq mail-user-agent 'notmuch-user-agent)

  ;; Tags that mean "where it is" (inbox, deleted) become folder moves on the
  ;; server at the next sync; see email/notmuch-hooks/pre-new.
  (setq notmuch-archive-tags '("-inbox" "-unread")
        notmuch-show-mark-read-tags '("-unread")
        notmuch-search-oldest-first nil
        notmuch-show-relative-dates t
        notmuch-mua-cite-function #'message-cite-original-without-signature
        notmuch-maildir-use-notmuch-insert t)

  (let ((account (night/mail-account)))
    (cond
     (account
      ;; `notmuch insert' tags and files the copy in one step; mbsync then
      ;; uploads it to the server's Sent and Drafts.
      (setq notmuch-fcc-dirs (format "%s/Sent +sent -inbox -unread" account)
            notmuch-draft-folder (format "%s/Drafts" account)))
     (t
      (display-warning
       'night-email
       "no mail account under the notmuch mail root, so sent mail will not be saved; see docs/email.md"))))

  ;; `tag:deleted' is in search.exclude_tags, but naming it in a query lifts
  ;; the exclusion, which is what the trash search relies on.
  (setq notmuch-saved-searches
        '((:name "inbox"   :query "tag:inbox"   :key "i" :sort-order newest-first)
          (:name "unread"  :query "tag:unread"  :key "u" :sort-order newest-first)
          (:name "flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
          (:name "todo"    :query "tag:todo"    :key "t" :sort-order newest-first)
          (:name "waiting" :query "tag:waiting" :key "w" :sort-order newest-first)
          (:name "sent"    :query "tag:sent"    :key "s" :sort-order newest-first)
          (:name "drafts"  :query "tag:draft"   :key "d" :sort-order newest-first)
          (:name "all"     :query "*"           :key "a" :sort-order newest-first)
          (:name "trash"   :query "tag:deleted" :key "D" :sort-order newest-first)))

  ;; The `k' menu (`notmuch-tag-jump').
  (setq notmuch-tagging-keys
        `((,(kbd "a") notmuch-archive-tags "Archive")
          (,(kbd "d") ("+deleted" "-inbox") "Delete (to Trash)")
          (,(kbd "f") ("+flagged") "Flag")
          (,(kbd "F") ("-flagged") "Unflag")
          (,(kbd "r") notmuch-show-mark-read-tags "Mark read")
          (,(kbd "u") ("+unread") "Mark unread")
          (,(kbd "t") ("+todo") "Todo")
          (,(kbd "T") ("-todo") "Done")
          (,(kbd "w") ("+waiting") "Waiting")
          (,(kbd "W") ("-waiting") "No longer waiting")))

  (map! :map (notmuch-hello-mode-map notmuch-search-mode-map notmuch-tree-mode-map notmuch-show-mode-map)
        :localleader
        :desc "Compose" "c" #'notmuch-mua-new-mail
        :desc "Sync mail" "u" #'night/mail-sync
        :desc "Search (consult)" "/" #'consult-notmuch)
  (map! :map notmuch-show-mode-map
        :localleader
        :desc "HTML in an Org buffer" "o" #'night/notmuch-show-html-in-org
        :desc "Toggle HTML renderer" "h" #'night/notmuch-toggle-html-renderer))

(after! (org notmuch)
  ;; `notmuch:id:' links from `org-store-link' and the "e" capture template.
  (require 'ol-notmuch))

(after! message
  ;; msmtp picks the account from the From header and reads the password from
  ;; the Keychain; see email/templates/msmtp-config.tmpl.
  (setq message-send-mail-function #'message-send-mail-with-sendmail
        sendmail-program (or (executable-find "msmtp") "/opt/homebrew/bin/msmtp")
        message-sendmail-envelope-from 'header
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-kill-buffer-on-exit t)

  (map! :map message-mode-map
        :localleader
        :desc "HTML from Org markup" "h" #'org-mime-htmlize
        :desc "Edit body in Org" "e" #'org-mime-edit-mail-in-org-mode))

(after! org-mime
  ;; Build the HTML part with message-mode's MML, which notmuch sends as is.
  (setq org-mime-library 'mml))
;;;
(night/set-leader-keys "o m" #'notmuch "Mail (notmuch)")

;;; night-gmail.el -*- lexical-binding: t; -*-

(defun night/h-gmail-launcher ()
  "Return the path to the installed Gmail profile launcher."
  (let ((launcher (expand-file-name "~/.local/share/night-gmail/night-gmail")))
    (unless (file-executable-p launcher)
      (user-error "The local Gmail profile needs setup (see docs/email.md)"))
    launcher))

(defun night/h-gmail-run (args &optional on-success)
  "Run the Gmail launcher with ARGS, then call ON-SUCCESS if it exits cleanly."
  (let ((default-directory (expand-file-name "~/"))
        (launcher (night/h-gmail-launcher))
        (operation (car args)))
    (make-process
     :name (concat "night-gmail-" operation)
     :buffer (generate-new-buffer (format " *night-gmail-%s*" operation))
     :command (cons launcher args)
     :noquery t
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (let ((buffer (process-buffer process)))
           (cond
            ((zerop (process-exit-status process))
             (when (buffer-live-p buffer) (kill-buffer buffer))
             (when on-success (funcall on-success)))
            (t
             (when (buffer-live-p buffer) (display-buffer buffer))
             (message "Gmail %s failed" operation)))))))))

(defun night/gmail ()
  "Open the selected Gmail account in its own Notmuch window."
  (interactive)
  (night/h-gmail-run '("emacs")))

(defun night/gmail-login (address)
  "Authorize Gmail ADDRESS in the browser and open its local inbox."
  (interactive "sGmail address: ")
  (night/h-gmail-run (list "login" address) #'night/gmail)
  (message "Complete Gmail sign-in in your browser"))

(defun night/gmail-switch-account ()
  "Choose an already authorized Gmail account and open its inbox."
  (interactive)
  (let* ((default-directory (expand-file-name "~/"))
         (launcher (night/h-gmail-launcher))
         (accounts (process-lines launcher "accounts")))
    (unless accounts (user-error "Authorize a Gmail account with M-x night/gmail-login"))
    (let ((address (completing-read "Gmail account: " accounts nil t)))
      (unless (equal address "")
        (night/h-gmail-run (list "select" address) #'night/gmail)))))

(defun night/gmail-pull ()
  "Download mail for the selected Gmail account in the background."
  (interactive)
  (night/h-gmail-run '("pull")
                     (lambda () (message "Gmail download finished. Refresh its Notmuch window")))
  (message "Downloading Gmail in the background"))

;;; night-gmail.el -*- lexical-binding: t; -*-

(defun night/gmail ()
  "Open the local Gmail inbox in its own Notmuch window.
The Gmail reader runs in a separate Emacs process with its own mail index."
  (interactive)
  (let ((launcher (expand-file-name "~/.local/share/night-gmail/night-gmail")))
    (unless (file-executable-p launcher)
      (user-error "The local Gmail profile needs setup (see docs/email.md)"))
    (make-process :name "night-gmail"
                  :buffer " *night-gmail*"
                  :command (list launcher "emacs")
                  :noquery t
                  :sentinel (lambda (process _event)
                              (when (and (memq (process-status process) '(exit signal))
                                         (not (zerop (process-exit-status process))))
                                (display-buffer (process-buffer process))
                                (message "Gmail reader failed to open"))))))

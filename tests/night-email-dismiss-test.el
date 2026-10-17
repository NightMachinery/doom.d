;;; -*- lexical-binding: t; -*-
;; Run with -L pointing at the pinned notmuch checkout's emacs directory.
(setq native-comp-enable-subr-trampolines nil)
(require 'ert)
(require 'cl-lib)
(require 'notmuch-show)
(defmacro after! (&rest _) nil)
(defun night/set-leader-keys (&rest _) nil)
(load (expand-file-name "../autoload/night-email.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun night-test/mail-thread ()
  "Insert a synthetic thread using real notmuch message properties."
  (let (regions)
    (dolist (id '("older" "arrival" "collapsed"))
      (let ((start (point)))
        (insert id "\nSynthetic message body.\n")
        (let ((end (point)))
          (add-text-properties start end (list :notmuch-message-extent (cons start end)))
          (put-text-property start (1+ start) :notmuch-message-properties
                             (list :id id :tags '("unread")
                                   :message-visible (not (equal id "collapsed"))))
          (push (cons start end) regions))))
    (nreverse regions)))

(ert-deftest night-mail-open-thread-dismisses-visible-arrival-away-from-point ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((regions (night-test/mail-thread))
             (system-type 'darwin)
             (major-mode 'notmuch-show-mode)
             (notmuch-show-mark-read-function #'ignore)
             (night/mail-dismiss-notifications-on-open t)
             captured)
        (goto-char (caar regions))
        ;; The cursor remains on an older, already acknowledged message.
        (notmuch-show-set-prop :night-notification-dismissed t)
        (advice-add 'notmuch-show-command-hook :after #'night/h-mail-dismiss-visible-notifications)
        (unwind-protect
            (cl-letf (((symbol-function 'window-end) (lambda (&rest _) (point-max)))
                      ((symbol-function 'make-process)
                       (lambda (&rest args) (push (plist-get args :command) captured))))
              (notmuch-show-command-hook)
              (notmuch-show-command-hook)
              (should (equal (mapcar #'car (mapcar #'last captured))
                             (list (secure-hash 'sha256 "arrival"))))
              (should (= (point) (caar regions)))
              (goto-char (caadr regions))
              (should (notmuch-show-get-prop :night-notification-dismissed))
              ;; Dismissal does not change the user's read/tag policy.
              (should (member "unread" (notmuch-show-get-tags)))
              (goto-char (car (nth 2 regions)))
              (should-not (notmuch-show-get-prop :night-notification-dismissed)))
          (advice-remove 'notmuch-show-command-hook #'night/h-mail-dismiss-visible-notifications))))))

(ert-deftest night-mail-dismiss-keeps-off-screen-messages-and-preserves-point ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (let* ((regions (night-test/mail-thread))
             (system-type 'darwin)
             (major-mode 'notmuch-show-mode)
             (night/mail-dismiss-notifications-on-open t)
             (start (+ 5 (caar regions)))
             (end (caadr regions))
             captured)
        (goto-char start)
        (cl-letf (((symbol-function 'window-start) (lambda (&rest _) start))
                  ((symbol-function 'window-end) (lambda (&rest _) end))
                  ((symbol-function 'make-process)
                   (lambda (&rest args) (push (plist-get args :command) captured))))
          (night/h-mail-dismiss-visible-notifications)
          (should (equal (mapcar #'car (mapcar #'last captured))
                         (list (secure-hash 'sha256 "older"))))
          (should (= (point) start))
          (goto-char (caadr regions))
          (should-not (notmuch-show-get-prop :night-notification-dismissed)))))))

(ert-run-tests-batch-and-exit)

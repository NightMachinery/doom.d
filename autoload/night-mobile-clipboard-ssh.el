;;; night-mobile-clipboard-ssh.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom night/mobile-clipboard-ssh-cache-ttl 900
  "Seconds to cache successful SSH clipboard readiness."
  :type 'natnum :group 'xterm)

(defcustom night/mobile-clipboard-ssh-failure-ttl 30
  "Seconds to cache failed SSH clipboard readiness or transfers."
  :type 'natnum :group 'xterm)

(defcustom night/mobile-clipboard-ssh-timeout 15
  "Maximum seconds for one SSH clipboard check or transfer."
  :type 'number :group 'xterm)

(defvar night/h-mobile-clipboard-ssh-cache (make-hash-table :test #'equal)
  "Host alias to (READY . EXPIRY); memory only, per Emacs daemon.")
(defvar night/h-mobile-clipboard-ssh-jobs (make-hash-table :test #'equal)
  "Host alias to a plist containing :busy and the latest :pending copy.")

(defun night/mobile-clipboard-cache-clear (&optional host)
  "Forget SSH readiness for HOST, or every host when omitted.
This does not cancel an in-flight copy.  Its result may populate the cache.
The next oversized copy checks readiness again."
  (interactive)
  (cond (host (remhash host night/h-mobile-clipboard-ssh-cache))
        (t (clrhash night/h-mobile-clipboard-ssh-cache)))
  (when (called-interactively-p 'interactive)
    (message "Mobile clipboard SSH readiness cache cleared"))
  t)

(defun night/h-mobile-clipboard-ssh-ready (host)
  "Return ready, down, or nil for an expired/missing HOST cache entry."
  (let ((entry (gethash host night/h-mobile-clipboard-ssh-cache)))
    (when (and entry (< (float-time) (cdr entry)))
      (cond ((car entry) 'ready) (t 'down)))))

(cl-defun night/h-mobile-clipboard-ssh-cache-put (&key host ready)
  "Record HOST's current READY state."
  (puthash host (cons ready (+ (float-time)
                              (cond (ready night/mobile-clipboard-ssh-cache-ttl)
                                    (t night/mobile-clipboard-ssh-failure-ttl))))
           night/h-mobile-clipboard-ssh-cache))

(cl-defun night/h-mobile-clipboard-ssh-run (&key host text callback)
  "Run an asynchronous readiness probe or copy to HOST.
Nil TEXT probes; a string is sent via UTF-8 stdin, never shell arguments.
CALLBACK receives a boolean success value exactly once.  Process output is
discarded so errors cannot accidentally log clipboard text."
  (let ((default-directory temporary-file-directory)
        process timer done)
    (cl-labels
        ((finish (ok)
           (unless done
             (setq done t)
             (when timer (cancel-timer timer))
             (funcall callback ok))))
      (condition-case nil
          (progn
            (setq process
                  (make-process
                   :name "mobile-clipboard-ssh" :buffer nil :noquery t
                   :connection-type 'pipe :coding '(utf-8-unix . utf-8-unix)
                   :command
                   (list "ssh" "-T" "-o" "BatchMode=yes"
                         "-o" "StrictHostKeyChecking=yes"
                         "-o" "ConnectTimeout=3" "-o" "ConnectionAttempts=1"
                         "-o" "ServerAliveInterval=3" "-o" "ServerAliveCountMax=1"
                         "-o" "ClearAllForwardings=yes" "--" host
                         (cond (text "termux-clipboard-set")
                               (t "command -v termux-clipboard-set >/dev/null")))
                   :filter (lambda (&rest _ignore))
                   :sentinel
                   (lambda (proc _event)
                     (when (memq (process-status proc) '(exit signal))
                       (finish (and (eq (process-status proc) 'exit)
                                    (= (process-exit-status proc) 0)))))))
            (setq timer
                  (run-at-time night/mobile-clipboard-ssh-timeout nil
                               (lambda ()
                                 (unless done
                                   ;; Mark finished before deleting: a deletion
                                   ;; sentinel must not run the callback twice.
                                   (setq done t)
                                   (when (process-live-p process)
                                     (delete-process process))
                                   (funcall callback nil)))))
            (when text (process-send-string process text))
            (process-send-eof process))
        (error
         (unless done
           (setq done t)
           (when timer (cancel-timer timer))
           (when (and process (process-live-p process)) (delete-process process))
           (funcall callback nil)))))))

(cl-defun night/h-mobile-clipboard-ssh-enqueue (&key host text)
  "Schedule TEXT from the selected frame for HOST, keeping the latest copy."
  (let ((job (or (gethash host night/h-mobile-clipboard-ssh-jobs)
                 (list :busy nil :pending nil))))
    (setf (plist-get job :pending) (cons text (selected-frame)))
    (puthash host job night/h-mobile-clipboard-ssh-jobs)
    (night/h-mobile-clipboard-ssh-drain host)))

(defun night/h-mobile-clipboard-ssh-drain (host)
  "Dispatch the latest pending HOST copy when no previous operation is active."
  (let* ((job (gethash host night/h-mobile-clipboard-ssh-jobs))
         (pending (plist-get job :pending)))
    (when (and pending (not (plist-get job :busy)))
      (setf (plist-get job :pending) nil)
      (let* ((text (car pending)) (frame (cdr pending))
             (large (> (string-bytes (encode-coding-string text 'utf-8-unix))
                       night/mobile-clipboard-max-bytes))
             (ready (night/h-mobile-clipboard-ssh-ready host)))
        (cond
         ((not (frame-live-p frame)) nil)
         ((not large)
          (with-selected-frame frame (night/h-mobile-clipboard-osc52-cut text)))
         ((eq ready 'down)
          (message "Mobile clipboard: SSH unavailable (cached); text remains in kill ring. Use M-x night/mobile-clipboard-cache-clear to retry now"))
         (t
          (setf (plist-get job :busy) t)
          (unless ready (setf (plist-get job :pending) pending))
          (night/h-mobile-clipboard-ssh-run
           :host host :text (and ready text)
           :callback
           (lambda (ok)
             (night/h-mobile-clipboard-ssh-cache-put :host host :ready ok)
             (setf (plist-get job :busy) nil)
             (when ready
               (message (cond (ok "Mobile clipboard: copied via SSH")
                              (t "Mobile clipboard: SSH copy failed; text remains in kill ring"))))
             ;; A newer small copy goes out after the old large copy completes.
             ;; Failed large copies are discarded, never replayed on recovery.
             (night/h-mobile-clipboard-ssh-drain host)))))))))

(provide 'night-mobile-clipboard-ssh)

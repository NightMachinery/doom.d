;;; autoload/night-redis.el -*- lexical-binding: t; -*-
;;;
(defvar night/redis-auth-file "~/.redis-auth"
  "File holding the redis =requirepass= password, one line, mode 600.
The zsh side reads the same file: =h-redis-auth-ensure= exports it as
REDISCLI_AUTH for =redism=.")

;; `night/redis-connected-p' used to be a variable, set here and again at the
;; end of `night/redis-connect'.  Nothing in the config ever read it, and it
;; went stale the instant `eredis-sentinel' dropped the process: a daemon would
;; report `t' while its connection was dead, which is worse than reporting
;; nothing at all.  It asks the process now.  The old binding is removed
;; explicitly because `defvar' does not reset an already-bound variable, so a
;; long-lived daemon would otherwise keep handing back the value it cached
;; before this change.
(when (boundp 'night/redis-connected-p)
  (makunbound 'night/redis-connected-p))

(defun night/redis-connected-p ()
  "Non-nil if there is a live connection to redis right now.

Asks the process rather than remembering an answer.  `eredis-sentinel'
nils `eredis--current-process' out whenever the connection closes and
announces it to nobody, so any cached answer is only ever a guess about
the past."
  ;; `and t' because `process-live-p' is a `memq', so it hands back the tail
  ;; of its status list rather than a boolean.  True enough for `if', but this
  ;; is a predicate people read the value of.
  (and (boundp 'eredis--current-process)
       (processp eredis--current-process)
       (process-live-p eredis--current-process)
       t))

(defun night/redis-password ()
  "The redis password from `night/redis-auth-file', or nil if unreadable."
  (let ((file (expand-file-name night/redis-auth-file)))
    (when (file-readable-p file)
      (let ((password
             (s-trim (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string)))))
        (unless (string-empty-p password)
          password)))))

(defun night/redis-connect ()
  "Connect to redis, AUTH, and confirm with PING.

Never signals: a downed redis warns and returns nil rather than breaking
startup. `eredis-connect' has no password argument of its own, so the AUTH has
to be a separate command; without it every call returns the string \"NOAUTH
Authentication required.\" as if it were data, because eredis parses error
replies exactly like status replies. See docs/redis-eredis-auth.md."
  (interactive)
  (condition-case err
      (let ((proc (eredis-connect "localhost" 6379)))
        (setq redis-connection-0 proc)
        (if-let ((password (night/redis-password)))
            (let ((reply (eredis-auth password)))
              ;; A server with no requirepass answers AUTH with an error; that
              ;; is fine, the PING below is what decides.
              (unless (or (equal reply "OK")
                          (and (stringp reply)
                               (s-prefix? "ERR Client sent AUTH" reply)))
                (warn "night/redis-connect: AUTH failed: %s" reply)))
          (warn "night/redis-connect: no password in %s, skipping AUTH"
                night/redis-auth-file))
        (let* ((pong (eredis-ping))
               (connected (equal pong "PONG")))
          (unless connected
            (warn "night/redis-connect: PING returned %S" pong))
          (and connected proc)))
    (error
     (warn "night/redis-connect: %s" (error-message-string err))
     nil)))

(defun night/redis-reconnect ()
  "Drop the current eredis connection and connect again, with AUTH.

The checked wrappers now recover from a dropped connection by themselves,
reconnecting once and retrying, so this is no longer needed merely because
redis was restarted.  It stays the remedy for the cases a retry cannot
reach: a rotated password, which must be re-read from
`night/redis-auth-file', and forcing a fresh connection when you want to
prove one works rather than inheriting a grandfathered one."
  (interactive)
  (ignore-errors (eredis-disconnect))
  (night/redis-connect))
;;;
;; eredis returns RESP error replies as ordinary strings and never signals:
;; `eredis-parse-error-response' is literally `eredis-parse-status-response',
;; so "-NOAUTH ..." arrives looking exactly like "+OK". These wrappers are how
;; a failure becomes visible instead of being mistaken for data.
(define-error 'night/redis-error "redis error")

(defun night/redis--check-status (reply expected what)
  "Return REPLY, or signal `night/redis-error' unless it equals EXPECTED.
Exact, not a heuristic: the success reply of these commands is a fixed string."
  (unless (equal reply expected)
    (signal 'night/redis-error (list what reply)))
  reply)

(defun night/redis--check-integer (reply what)
  "Return REPLY, or signal `night/redis-error' unless it is an integer.
Exact, not a heuristic: eredis returns integer replies as elisp integers and
error replies as strings, so the types cannot collide."
  (unless (integerp reply)
    (signal 'night/redis-error (list what reply)))
  reply)

(defconst night/redis-error-reply-regexp
  (concat "\\`"
          (regexp-opt '("NOAUTH" "WRONGPASS" "NOPERM" "WRONGTYPE" "ERR"
                        "LOADING" "READONLY" "OOM" "MISCONF" "MASTERDOWN"
                        "CLUSTERDOWN" "MOVED" "EXECABORT"))
          " ")
  "Redis error-code prefixes, for bulk-string replies only.

Unlike the two checks above this one is a heuristic, because a bulk string
carries no type marker once eredis has stripped the leading =-=: a stored
value that begins with \"ERR \" would be rejected as an error. Only use it
where the value domain makes that implausible (paths, cached passwords).")

(defun night/redis--error-reply-p (reply)
  (and (stringp reply)
       (string-match-p night/redis-error-reply-regexp reply)))

(defconst night/redis--not-connected-message "redis not connected"
  "What eredis signals when it has no live connection.

`eredis-get-process' raises a plain `error' carrying this text and
nothing more structured, so matching the message is the only handle on
offer.  Fragile by nature: if eredis ever defines a real error symbol,
switch to that and delete this.")

(defun night/redis--not-connected-error-p (err)
  "Non-nil if ERR is eredis complaining that there is no connection."
  (and (eq (car-safe err) 'error)
       (let ((message (car-safe (cdr-safe err))))
         (and (stringp message)
              (string-match-p
               (regexp-quote night/redis--not-connected-message)
               message)))))

(defun night/redis--call-with-reconnect (fn)
  "Call FN; if the connection is dead, reconnect once and call it again.

Nothing re-dials on its own -- `eredis-sentinel' nils the process out
and deletes it -- so before this, a redis restart or a dropped socket
left every later call failing until someone ran `night/redis-reconnect'
by hand.  Following an `audiofile:' link was usually how that got
noticed.

Only a connection error retries.  Redis's own refusals -- NOAUTH,
WRONGPASS, WRONGTYPE -- arrive from eredis as ordinary strings rather
than signals, so they never reach here at all; they travel on to the
checked wrappers below and signal `night/redis-error' there.  That
separation is what makes the retry safe: a wrong password cannot put
this into a loop, because it does not look like a failure to this code.

The retry is exactly one.  `night/redis-connect' does not signal when
redis is down, so a second attempt fails the same way and propagates,
rather than reconnecting forever."
  (condition-case err
      (funcall fn)
    (error
     (unless (night/redis--not-connected-error-p err)
       (signal (car err) (cdr err)))
     (night/redis-connect)
     (funcall fn))))

(defmacro night/redis-with-reconnect (&rest body)
  "Run BODY, reconnecting once and retrying if the connection is dead.

Wrap only the raw eredis call, never the check around it: the checkers
turn an error *reply* into a signal, and a signal raised by them must
not be mistaken for a dead connection.  See
`night/redis--call-with-reconnect'."
  (declare (indent 0) (debug t))
  `(night/redis--call-with-reconnect (lambda () ,@body)))

(defun night/redis-set (key value)
  "SET KEY to VALUE, signalling `night/redis-error' unless redis replies OK."
  (night/redis--check-status
   (night/redis-with-reconnect (eredis-set key value))
   "OK" (format "SET %s" key)))

(defun night/redis-setnx (key value)
  "SETNX KEY to VALUE, returning 1 or 0 and signalling on an error reply."
  (night/redis--check-integer
   (night/redis-with-reconnect (eredis-setnx key value))
   (format "SETNX %s" key)))

(defun night/redis-expire (key seconds)
  "EXPIRE KEY after SECONDS, signalling on an error reply."
  (night/redis--check-integer
   (night/redis-with-reconnect (eredis-expire key seconds))
   (format "EXPIRE %s" key)))

(defun night/redis-get (key)
  "GET KEY, signalling `night/redis-error' if the reply looks like an error.
See `night/redis-error-reply-regexp' for why this one can be fooled."
  (let ((reply (night/redis-with-reconnect (eredis-get key))))
    (when (night/redis--error-reply-p reply)
      (signal 'night/redis-error (list (format "GET %s" key) reply)))
    reply))
;;;
(defun night/redis-lock-key (key)
  "Generate the Redis lock key for KEY."
  (format "emacs_lock::%s" key))

(cl-defun eredis-retry (&key max sleep fn args)
  "Retry FN with ARGS until it returns 1 or MAX times.
Sleep for SLEEP seconds between retries."
  (let ((retries 0)
        (result nil))
    (while (and (or (null max) (< retries max))
                (not (eq (setq result (apply fn args)) 1)))
      (sleep-for sleep)
      (cl-incf retries))
    (eq result 1)))

(cl-defun night/redis-lock
    (key &key (timeout (* 3600 72)) (max-retries 10) (sleep 0.1))
  "Acquire a lock on KEY with an optional TIMEOUT in seconds, MAX-RETRIES, and SLEEP."
  (let ((lock-key (night/redis-lock-key key)))
    ;; Checked calls on purpose: with raw `eredis-setnx' an auth or connection
    ;; failure comes back as a string, never as 1, so the retry loop burnt its
    ;; whole budget and then reported "lock not acquired" for what was really a
    ;; broken connection.
    (when (eredis-retry :sleep sleep :max max-retries :fn #'night/redis-setnx :args (list lock-key (number-to-string (float-time))))
      (when timeout
        (night/redis-expire lock-key timeout)))))

(defun night/redis-unlock (key)
  "Release the lock on KEY."
  (let ((lock-key (night/redis-lock-key key)))
    (eredis-del lock-key)))

(cl-defmacro with-redis-lock ((&key key-name (timeout nil)) &body body)
  "Execute BODY holding a Redis lock on KEY-NAME, with an optional TIMEOUT.

Signals if the lock cannot be taken, rather than running BODY unlocked as this
used to: an unheld lock is indistinguishable from a held one to the body, so
failing quietly defeats the point of locking at all."
  `(if (night/redis-lock ,key-name :timeout ,timeout)
       (unwind-protect
           (progn ,@body)
         (night/redis-unlock ,key-name))
     (error "with-redis-lock: could not acquire lock: %s" ,key-name)))
(comment
 (ert-deftest test-night/redis-lock-key ()
   (should (equal (night/redis-lock-key "my-key") "emacs_lock::my-key")))

 (ert-deftest test-eredis-retry ()
   (let ((counter 0))
     (should-not (eredis-retry :max 3 :sleep 0.1 :fn (lambda () (cl-incf counter) (= counter 4)) :args nil))
     (should (= counter 3)))
   (let ((counter 0))
     (should (eredis-retry :max 3 :sleep 0.1 :fn
                           (lambda ()
                             (cl-incf counter)
                             (when (= counter 2)
                               1))
                           :args nil))
     (should (= counter 2))))

 (ert-deftest test-night/redis-lock ()
   (let ((key "test-lock"))
     (should (night/redis-lock key :timeout 1 :max-retries 1 :sleep 0.1))
     (should-not (night/redis-lock key :timeout 1 :max-retries 1 :sleep 0.1))
     (sleep-for 1.1)
     (should (night/redis-lock key :timeout 1 :max-retries 1 :sleep 0.1))))

 (ert-deftest test-night/redis-unlock ()
   (let ((key "test-unlock"))
     (should (night/redis-lock key :timeout 1 :max-retries 1 :sleep 0.1))
     (night/redis-unlock key)
     (should (night/redis-lock key :timeout 1 :max-retries 1 :sleep 0.1))))

 (ert-deftest test-with-redis-lock ()
   (let ((key "test-macro-lock")
         (counter 0))
     (with-redis-lock (:key-name key :timeout 1)
                      (cl-incf counter)
                      (should (= counter 1))
                      (should-not (night/redis-lock key :timeout 1 :max-retries 1 :sleep 0.1)))
     (should (night/redis-lock key :timeout 1 :max-retries 1 :sleep 0.1))))
 (ert-run-tests-interactively t)
;; To run the tests, evaluate the test definitions and then call `(ert-run-tests-interactively t)` or run the tests individually with `(ert-run-test 'test-name)`.
 )
;;;
(night/redis-connect)
;;;
(provide 'night-redis)

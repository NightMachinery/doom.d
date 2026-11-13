;;; tests/night-redis-test.el -*- lexical-binding: t; -*-
(setq native-comp-enable-subr-trampolines nil)
(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defmacro comment (&rest _body) nil)

;; Minimal `s' stubs so `autoload/night-redis.el' loads in batch without straight.
(unless (fboundp 's-trim)
  (defun s-trim (s) (string-trim s)))
(unless (fboundp 's-prefix?)
  (defun s-prefix? (prefix s) (string-prefix-p prefix s)))

;; Stub `eredis' before loading `autoload/night-redis.el'.
(defvar eredis--current-process nil)
(defvar eredis-max-retries 10)
(defvar eredis-response-timeout 3)
(defvar redis-connection-0 nil)
(defun eredis-connect (_host _port &optional _nowait) nil)
(defun eredis-auth (_pw) "OK")
(defun eredis-ping () "PONG")
(defun eredis-disconnect (&optional _proc) nil)
(defun eredis-delete-process (&optional proc)
  (let ((target (or proc eredis--current-process)))
    (when (eq target eredis--current-process)
      (setq eredis--current-process nil))
    (when (processp target)
      (delete-process target))))
(defun eredis-parse-response (_resp) (error "stub"))
(defun eredis-get-response (_proc) (error "stub"))
(defun eredis-command-returning (_cmd &rest _args) (error "stub"))
(defun eredis-set (_k _v) "OK")
(defun eredis-setnx (_k _v) 1)
(defun eredis-expire (_k _s) 1)
(defun eredis-get (_k) "val")
(defun eredis-del (&rest _keys) 1)
(provide 'eredis)

(cl-letf (((symbol-function 'eredis-connect) (lambda (&rest _) nil))
          ((symbol-function 'warn) #'ignore))
  (load (expand-file-name "../autoload/night-redis.el"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(ert-deftest night/eredis-parse-response-empty-returns-incomplete ()
  (should (equal (night/eredis-parse-response--empty "") '(incomplete . 0)))
  (should (equal (night/eredis-parse-response--empty nil) '(incomplete . 0)))
  (should-not (night/eredis-parse-response--empty "+OK\r\n")))

(ert-deftest night/eredis-get-response-dead-process-signals-not-connected ()
  "An EOF/dead process with an empty buffer signals `redis not connected' and cleans up."
  (let* ((buf (generate-new-buffer " *test-redis-dead*"))
         (proc (make-pipe-process :name "test-redis-dead"
                                  :buffer buf
                                  :command '("true")))
         (deleted nil))
    (unwind-protect
        (progn
          (process-put proc 'response-start 1)
          (setq eredis--current-process proc)
          (cl-letf (((symbol-function 'accept-process-output)
                     (lambda (&rest _) nil))
                    ((symbol-function 'process-live-p)
                     (lambda (_p) nil))
                    ((symbol-function 'eredis-delete-process)
                     (lambda (p)
                       (setq deleted p)
                       (when (eq p eredis--current-process)
                         (setq eredis--current-process nil))
                       (when (processp p) (ignore-errors (delete-process p))))))
            (let ((err (should-error (night/eredis-get-response proc) :type 'error)))
              (should (string-match-p "redis not connected" (error-message-string err)))
              (should (eq deleted proc))
              (should-not eredis--current-process)
              (should-not (buffer-live-p buf)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest night/eredis-get-response-timeout-drops-process-fast ()
  "Zero-byte timeout on an open socket fails on the first timeout and cleans up."
  (let* ((buf (generate-new-buffer " *test-redis-timeout*"))
         (proc (make-pipe-process :name "test-redis-timeout"
                                  :buffer buf
                                  :command '("cat")))
         (accept-calls 0)
         (eredis-max-retries 1000))
    (unwind-protect
        (progn
          (process-put proc 'response-start 1)
          (setq eredis--current-process proc)
          (cl-letf (((symbol-function 'accept-process-output)
                     (lambda (&rest _)
                       (cl-incf accept-calls)
                       nil))
                    ((symbol-function 'process-live-p)
                     (lambda (_p) t)))
            (let ((err (should-error (night/eredis-get-response proc) :type 'error)))
              (should (string-match-p "timed out" (error-message-string err)))
              (should (= accept-calls 1))
              (should-not eredis--current-process)
              (should-not (buffer-live-p buf)))))
      (when (processp proc) (ignore-errors (delete-process proc)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest night/redis-reconnect-retries-at-most-once-even-when-buggy ()
  "A broken or non-recovering `night/redis-connect' cannot trigger an infinite loop."
  (let ((calls 0)
        (connects 0))
    (cl-letf (((symbol-function 'night/redis-connect)
               (lambda ()
                 (cl-incf connects)
                 ;; Even if `night/redis-connect' mistakenly invokes a wrapped call,
                 ;; `night/redis--reconnecting-p' blocks nested retries.
                 (ignore-errors
                   (night/redis-with-reconnect
                     (error "redis not connected")))
                 nil)))
      (let ((err (should-error
                  (night/redis-with-reconnect
                    (cl-incf calls)
                    (error "redis not connected"))
                  :type 'error)))
        (should (string-match-p "redis not connected" (error-message-string err)))
        (should (= calls 2))
        (should (= connects 1))))))

(ert-deftest night/redis-reconnect-recovers-on-args-out-of-range-and-broken-pipe ()
  (dolist (err-fn (list (lambda () (signal 'args-out-of-range '("" 0)))
                        (lambda () (error "writing to process: Broken pipe, redis"))
                        (lambda () (error "Process redis-localhost-6379 not running"))))
    (let ((attempt 0)
          (connects 0))
      (cl-letf (((symbol-function 'night/redis-connect)
                 (lambda () (cl-incf connects) t)))
        (should (equal (night/redis-with-reconnect
                         (cl-incf attempt)
                         (if (= attempt 1)
                             (funcall err-fn)
                           "OK"))
                       "OK"))
        (should (= attempt 2))
        (should (= connects 1))))))

(ert-deftest night/redis-connect-cleans-up-on-ping-failure ()
  (let* ((buf (generate-new-buffer " *test-redis-ping-fail*"))
         (proc (make-pipe-process :name "test-redis-ping-fail"
                                  :buffer buf
                                  :command '("cat"))))
    (unwind-protect
        (cl-letf (((symbol-function 'eredis-connect)
                   (lambda (_h _p)
                     (setq eredis--current-process proc)
                     proc))
                  ((symbol-function 'night/redis-password) (lambda () "secret"))
                  ((symbol-function 'eredis-auth) (lambda (_pw) "OK"))
                  ((symbol-function 'eredis-ping) (lambda () "NOAUTH Authentication required."))
                  ((symbol-function 'warn) #'ignore))
          (should-not (night/redis-connect))
          (should-not eredis--current-process)
          (should-not (buffer-live-p buf)))
      (when (processp proc) (ignore-errors (delete-process proc)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; autoload/night-redis.el -*- lexical-binding: t; -*-
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
    (when (eredis-retry :sleep sleep :max max-retries :fn #'eredis-setnx :args (list lock-key (number-to-string (float-time))))
      (when timeout
        (eredis-expire lock-key timeout)))))

(defun night/redis-unlock (key)
  "Release the lock on KEY."
  (let ((lock-key (night/redis-lock-key key)))
    (eredis-del lock-key)))

(cl-defmacro with-redis-lock ((&key key-name (timeout nil)) &body body)
  "Execute BODY with a Redis lock on KEY-NAME and an optional TIMEOUT."
  `(progn
     (night/redis-lock ,key-name :timeout ,timeout)
     (unwind-protect
         (progn ,@body)
       (night/redis-unlock ,key-name))))
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
(provide 'night-redis)

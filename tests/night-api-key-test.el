;;; night-api-key-test.el --- optional startup credentials -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;; Loading the whole module with an absent optional credential must succeed.
(cl-letf (((symbol-function 'night/brishz)
           (lambda (&rest _) (signal 'end-of-file nil))))
  (load (expand-file-name "../autoload/night-openai.el"
                          (file-name-directory load-file-name)) nil t))

(ert-deftest night/api-key-optional-at-startup ()
  (dolist (fixture '("" missing "inert-sentinel"))
    (cl-letf (((symbol-function 'night/brishz)
               (lambda (&rest _)
                 (cond ((eq fixture 'missing) (signal 'end-of-file nil))
                       (t fixture)))))
      (should (equal (night/h-api-key-get "test-only")
                     (and (stringp fixture) (not (string-empty-p fixture)) fixture))))))

(ert-deftest night/api-key-other-errors-remain-visible ()
  (cl-letf (((symbol-function 'night/brishz)
             (lambda (&rest _) (error "inert bridge failure"))))
    (should-error (night/h-api-key-get "test-only") :type 'error)))

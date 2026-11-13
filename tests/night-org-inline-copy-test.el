;;; tests/night-org-inline-copy-test.el -*- lexical-binding: t; -*-
(setq native-comp-enable-subr-trampolines nil)
(require 'ert)
(require 'org)

(defmacro after! (_packages &rest body) `(progn ,@body))
(provide 'evil-traces)
(defun +org/dwim-at-point (&rest _) nil)

(load (expand-file-name "../autoload/org/night-org-inline-copy.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest night/org-inline-copy-around-dwim-does-not-retry-orig-on-error ()
  "An error inside `orig-fn' propagates once without retrying `orig-fn'."
  (let ((calls 0))
    (with-temp-buffer
      (org-mode)
      (insert "plain text\n")
      (goto-char (point-min))
      (let ((err (should-error
                  (night/org-inline-copy--around-dwim
                   (lambda (&rest _)
                     (cl-incf calls)
                     (error "boom from orig")))
                  :type 'error)))
        (should (equal (error-message-string err) "boom from orig"))
        (should (= calls 1))))))

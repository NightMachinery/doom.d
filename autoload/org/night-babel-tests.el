;;; autoload/org/night-babel-tests.el -*- lexical-binding: t; -*-

(defun night/babel-tests ()
  (interactive)
  (copy-to-buffer "*my-test*" (point-min) (point-max))
  (org-babel-execute-buffer)
  (night/diff-buffers "*my-test*" (current-buffer))
  ;; (diff-buffers "*my-test*" (current-buffer) (list "--unified=30"))
  )

;;; night-git.el ---                                 -*- lexical-binding: t; -*-
;;;
(defalias 'night/git-url-copy #'git-link)
;;;
(defcustom night/git-executable "git"
  "Path to the `git' executable."
  :type 'string)

(defun night/h--git-lines (dir &rest args)
  "Run git ARGS in DIR and return lines."
  (let ((default-directory (file-name-as-directory (expand-file-name dir))))
    (apply #'process-lines night/git-executable args)))

(defun night/h--git-toplevel (&optional dir)
  "Return the toplevel of the git repo containing DIR (or `default-directory')."
  (car (night/h--git-lines (or dir default-directory) "rev-parse" "--show-toplevel")))

;;;

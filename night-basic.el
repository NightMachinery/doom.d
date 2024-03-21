;;; ~/doom.d/night-basic.el -*- lexical-binding: t; -*-
(setenv "SHELL" "/bin/bash")

(setq shell-file-name "/bin/bash")

(setq explicit-shell-file-name "/bin/bash") ;;Set emacs shell to bash.

(setenv "PAGER" "cat")                      ;;Required for shell mode.

(defun night/set-leader-keys (keys fn &optional desc)
  (map! :leader
        :desc (or desc (format "%s" fn) "no description provided") keys fn))

(defun night/unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))
(defalias #'night/advice-remove-all #'night/unadvice)
;;;
(require 'eredis)
(setq redis-connection-0 (eredis-connect "localhost" 6379))
;;;
(defmacro night/defface (face spec &optional doc &rest args)
  "Define a face FACE with spec SPEC, optional documentation DOC, and custom ARGS.
If FACE is already defined, update its spec with SPEC."
  (declare (doc-string 3) (indent defun))
  (let ((face-symbol (if (symbolp face) face (eval face))))
    `(if (facep ',face-symbol)
         (face-spec-set ',face-symbol ,spec)
       (defface ,face ,spec ,doc ,@args))))
;;;

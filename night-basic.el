;;; ~/doom.d/night-basic.el -*- lexical-binding: t; -*-
(defgroup night nil
  "Customizations for Night utilities."
  :group 'tools
  :prefix "night/")
;;;
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
(defun night/bool (val)
  "Convert VAL to a boolean (t or nil)."
  (if val t nil))

(defun night/bool-smart (val)
  "Convert VAL to a boolean (t or nil).
If VAL is a sequence with zero length, return nil."
  (cond
   ;; Check if val is non-nil and a sequence with zero length.
   ((and (sequencep val) (zerop (length val)))
    nil)
   ;; Check if val is non-nil.
   (val t)
   ;; Otherwise, return nil.
   (t nil)))

(comment
 (night/bool "")
 (night/bool-smart ""))
;;;
(require 'eredis)
;; The connection itself is made by `night/redis-connect' in
;; autoload/night-redis.el, which needs to AUTH now that redis has a
;; requirepass. That file loads after this one (see night-loader.el), and
;; nothing in between touches redis at load time. The `require' stays here so
;; `(featurep 'eredis)' is already true for night-password.el's `after!'.
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

;;; night-alias.el -*- lexical-binding: t; -*-
;; `defalias' already works.
;; I used to think that advices attached to defalias-defined aliases also attach to the original function, but this is NOT true.
;; So there is no use for these alias-maker macros.
;;;
(defmacro night/h--alias-wrapper (new old interactive-p)
  "Create NEW as a wrapper around OLD.
If INTERACTIVE-P is non-nil, NEW is a command and delegates to OLD with
`call-interactively' when invoked via M-x."
  `(defun ,new (&rest args)
     ,(format "Alias wrapper for `%s`.\n\nWhen called programmatically, forwards ARGS to `%s`." old old)
     ,@(if interactive-p '((interactive)) nil)
     (let ((verbosity-level 0))
       (condition-case err
           (cond
            ,@(if interactive-p
                  `(((called-interactively-p 'interactive)
                     (call-interactively #',old))))
            ((null args)
             (funcall #',old))
            (t
             (apply #',old args)))
         (error
          (message "night: alias %s -> %s failed: %S" ',new ',old err)
          (signal (car err) (cdr err)))))))

(defmacro night/alias-interactive (new old)
  "Define NEW as an interactive wrapper for OLD."
  `(night/h--alias-wrapper ,new ,old t))

(defmacro night/alias-ni (new old)
  "Define NEW as a non-interactive wrapper for OLD."
  `(night/h--alias-wrapper ,new ,old nil))
;;;

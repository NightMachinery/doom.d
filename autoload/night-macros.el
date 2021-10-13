;;; autoload/night-macros.el -*- lexical-binding: t; -*-
;;;
(defmacro night/with-messages-suppressed (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated.
Also see [help:doom-shut-up-a]."
  (declare (indent 0))
  `(let ((inhibit-message t))
     (progn ,@body)))

(comment
 (night/with-messages-suppressed (z bello)
                                 (message "hi")
                                 nil)
 (progn (z bello)
                                 (message "hi")
                                 nil))
;;;

(sly-eval `(slynk-backend:describe-symbol-for-emacs "mapcar"))
(sly-eval `(cl:documentation 'mapcar 'function) :cl)

(sly-eval `(slynk:describe-to-string (slynk:parse-symbol-or-lose 'mapcar)) :cl)

;;;
(abbreviate-file-name
 (f-expand "~/tmp"))
;;;

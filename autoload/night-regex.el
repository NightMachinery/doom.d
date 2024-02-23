;;; night-regex.el ---                               -*- lexical-binding: t; -*-
;;; Code:
(defun night/regex-escape (pattern)
  (z regex-escape (identity pattern)))

(defun night/regex-escape-fast (pattern)
  "@seeAlso `night/regex-escape'"
  (--> pattern
       (regexp-quote it)
       ;; replace `|` with `\|`
       (replace-regexp-in-string "|" "\\\\|" it)))
(comment
 (night/regex-escape-fast "a|b"))

(defun night/regex-escape-smart (pattern)
  (cond
   (night/h-consult-ugrep-in-progress
    ;; (night/regex-escape pattern)
    ;; [[id:90e6d2fd-9259-441b-beca-41e408f9b090][{BUG} Escaped space causes an error · Issue #360 · Genivia/ugrep]]
    (night/regex-escape-fast pattern)
    )
   (t (regexp-quote pattern))))

;;; night-regex.el ends here
(provide 'night-regex)

;;; night-regex.el ---                               -*- lexical-binding: t; -*-
;;; Code:
(defun night/in-evil-ex-completion-p ()
  "Return non-nil if we are in evil-ex-completion."
  (let ((active-maps (current-active-maps)))
    (seq-contains-p active-maps evil-ex-completion-map
                    ;; #'equalp
                    )))

(defun night/in-evil-ex-search-p ()
  "Return non-nil if we are in evil-ex-search."
  (let ((active-maps (current-active-maps)))
    (seq-contains-p active-maps evil-ex-search-keymap
                    ;; #'equalp
                    )))

(defun night/regex-escape (pattern)
  (z regex-escape (identity pattern)))

(defun night/regex-escape-fast (pattern)
  "@seeAlso `night/regex-escape'"
  (--> pattern
       (regexp-quote it)
       ;; replace `|` with `\|`
       (replace-regexp-in-string "|" "\\\\|" it)))
(comment
 (night/regex-escape-fast "a|b/c"))

(defun night/regex-escape-smart (pattern)
  (let*
      ((escaped
        (cond
         (night/h-consult-ugrep-in-progress
          ;; (night/regex-escape pattern)
          ;; [[id:90e6d2fd-9259-441b-beca-41e408f9b090][{BUG} Escaped space causes an error · Issue #360 · Genivia/ugrep]]
          (night/regex-escape-fast pattern))
         (t (regexp-quote pattern))))
       (escaped
        (cond
         ((or
           (night/in-evil-ex-search-p)
           (night/in-evil-ex-completion-p))
          (replace-regexp-in-string "/" "\\\\/" escaped))
         (t escaped))))
    escaped))

;;; night-regex.el ends here
(provide 'night-regex)

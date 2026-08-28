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

;;;
(defvar night/h-regex-dialect nil
  "Regex dialect expected by the consumer of the current minibuffer input.

Search entry points let-bind this so that whatever we paste into their
minibuffer gets quoted for the right engine.  One of:

  `emacs'       Emacs regexp syntax.  This is what evil's `/' search,
                `consult-line' and consult's own grep builders want; the
                latter compile it down to the engine's syntax themselves.
  `pcre'        Perl-compatible syntax, handed to the engine verbatim.
  `ugrep-bool'  PCRE as ugrep reads it under `--bool', where an unescaped
                space is an AND operator and a leading `-' is NOT.

nil means `emacs', which is the right default for everything in Emacs.
See `night/regex-dialect', `night/regex-escape-smart'.")

(defun night/regex-dialect ()
  "Return the regex dialect the current minibuffer input is headed for."
  (or night/h-regex-dialect 'emacs))
;;;
(defun night/regex-escape (pattern)
  (z regex-escape (identity pattern)))

(defun night/regex-escape-fast (pattern)
  "Quote PATTERN for PCRE engines.
@seeAlso `night/regex-escape'"
  (--> pattern
       (regexp-quote it)
       ;; Replace `|` etc. with their escaped versions `\|` etc.:
       (replace-regexp-in-string "\\([\"()|{}]\\)" "\\\\\\1" it)))
(comment
 (night/regex-escape-fast "a|b/c)")
 ;; You can test with =-hi-\/man -wow\(ok?)= and [help:night/consult-ugrep-buffer].
 )

(defun night/regex-escape-ugrep (pattern)
  (let*
      ((escaped pattern)
       (escaped
          ;; (night/regex-escape pattern)
          ;; [[id:90e6d2fd-9259-441b-beca-41e408f9b090][{BUG} Escaped space causes an error · Issue #360 · Genivia/ugrep]]
        (-->
           escaped
           (night/regex-escape-fast it)

           ;; Trying to escape =--bool= syntax:
           (replace-regexp-in-string " " "[ ]" it)
           (replace-regexp-in-string "^-" "\\\\-" it))
        ))
    escaped))

(defun night/regex-escape-smart (pattern)
  "Quote PATTERN so that it matches itself literally.
Which quoting is correct depends on who reads the pattern, so this
dispatches on `night/regex-dialect'."
  (let*
      ((escaped
        (pcase (night/regex-dialect)
          ('ugrep-bool (night/regex-escape-ugrep pattern))
          ('pcre (night/regex-escape-fast pattern))
          ;; `regexp-quote' deliberately leaves `(){}|+' alone, as they are
          ;; literal in Emacs regexps. That is only safe as long as nothing
          ;; downstream re-reads the pattern in a dialect where they are not;
          ;; see `evil-ex-search-vim-style-regexp' in [help:night-evil].
          (_ (regexp-quote pattern))))
       (escaped
        (cond
         ((or
           (night/in-evil-ex-search-p)
           (night/in-evil-ex-completion-p))
          ;; evil splits the pattern from its offset at the first unescaped `/'.
          (replace-regexp-in-string "/" "\\\\/" escaped))
         (t escaped))))
    escaped))

(defun night/regex-group-shy (&rest alternatives)
  "Join ALTERNATIVES into one non-capturing group, in `night/regex-dialect'.
Emacs spells the group and the alternation with backslashes; PCRE does not."
  (pcase (night/regex-dialect)
    ((or 'pcre 'ugrep-bool)
     (concat "(?:" (string-join alternatives "|") ")"))
    (_
     (concat "\\(?:" (string-join alternatives "\\|") "\\)"))))
(comment
 (night/regex-group-shy "a" "b")
 (let ((night/h-regex-dialect 'ugrep-bool))
   (night/regex-group-shy "a" "b")))

;;; night-regex.el ends here
(provide 'night-regex)

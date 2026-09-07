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

;;;
(defvar night/h-pcre-cache (make-hash-table :test #'equal)
  "Memo table for `night/pcre-to-regexp', keyed by the PCRE source.")

(defun night/h-pcre-available-p ()
  "Make `pcre2el' loadable, and say whether it is.

`packages.el' declares it, but straight only puts an activated package
on `load-path'.  Until the next `doom sync' the build directory is
already on disk and merely unreferenced, so point at it rather than
leaving every PCRE unusable in the meantime.  After the sync the plain
`require' succeeds and this second arm never runs."
  (or (require 'pcre2el nil t)
      (when-let ((dir (car (file-expand-wildcards
                            (expand-file-name
                             "straight/build-*/pcre2el"
                             (or (bound-and-true-p straight-base-dir)
                                 "~/.emacs.d/.local/"))))))
        (add-to-list 'load-path dir)
        (require 'pcre2el nil t))))

(defun night/pcre-to-regexp (pattern)
  "Return PATTERN, written as a PCRE, as an Emacs regexp, or nil.

nil means the conversion could not be made -- `pcre2el' is unavailable,
or PATTERN uses something Emacs regexps cannot express, lookaround being
the usual one.  Callers must decide what an unusable pattern means for
them; what they must not do is fall back to reading PATTERN as an Emacs
regexp, because that quietly changes what it matches -- an alternation
group turns into six literal characters, and a pattern written to catch
something ends up catching nothing at all.

Memoised, failures included, because callers sit behind reflex
keybindings and must not pay `rxt-pcre-to-elisp' on every keystroke."
  (let ((cached (gethash pattern night/h-pcre-cache 'night/h-absent)))
    (if (not (eq cached 'night/h-absent))
        cached
      (puthash pattern
               (condition-case err
                   (cond
                    ;; pcre2el implements \\A and \\Z but not \\z, which it
                    ;; renders as a literal `z' without complaining -- so
                    ;; "\\.age\\z" comes back matching ".agez" and nothing
                    ;; ever hits it.  Refuse the pattern instead; \\Z is what
                    ;; anchors a path to its end here anyway.
                    ((let ((case-fold-search nil))
                       ;; Or this would fire on \\Z, the spelling we want.
                       (string-match-p "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\\\z" pattern))
                     (display-warning
                      'night/regex
                      (format "`%s' uses \\z, which pcre2el mistranslates; write \\Z instead."
                              pattern)
                      :error)
                     nil)
                    ((night/h-pcre-available-p)
                     (rxt-pcre-to-elisp pattern))
                    (t
                     (display-warning
                      'night/regex
                      "pcre2el is unavailable, so PCRE patterns cannot be used. Run `doom sync'."
                      :error)
                     nil))
                 (error
                  (display-warning
                   'night/regex
                   (format "cannot read `%s' as a PCRE: %s"
                           pattern (error-message-string err))
                   :error)
                  nil))
               night/h-pcre-cache))))
(comment
 (night/pcre-to-regexp "\\.(gpg|age)\\Z")
 (night/pcre-to-regexp "/private/(?!pub/)"))

;;; night-regex.el ends here
(provide 'night-regex)

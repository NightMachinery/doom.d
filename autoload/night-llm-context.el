;;; autoload/night-llm-context.el -*- lexical-binding: t; -*-
;;
;; What a command may read out of a buffer before sending it to a model, and
;; whether it may send at all.
;;
;; Shared by `night/fim-insert-at-point' in night-mistral-fim.el and by the
;; ellama commands in night-ellama.el.  It lives in its own file because those
;; two cannot depend on each other: night-mistral-fim.el is wrapped in
;; `(after! (night-openai night/ellama) ...)' and provides no feature at all,
;; so its body evaluates strictly after night-ellama.el's in the same `provide'
;; cascade.  Both depend on this instead.
;;
;; Loaded top level, so the only thing it needs at load time is
;; `night/defface', which night-basic.el has already provided -- night-loader.el
;; loads "basic" explicitly before it sweeps autoload/.  Everything else
;; (`night/pcre-to-regexp', `night/file-path-candidates',
;; `night/buffer-encrypted-p', `night/flash-region', `night/active-overlays')
;; is touched only inside function bodies, so their own load order is free.

(require 'cl-lib)

;; Declared for the byte-compiler; they live in night-ui.el, night-file.el and
;; night-regex.el, which this file only calls into at runtime.
(defvar night/active-overlays)
(declare-function night/file-path-candidates "night-file")
(declare-function night/pcre-to-regexp "night-regex")

;;;
(define-obsolete-variable-alias 'night/fim-path-policy
  'night/llm-path-policy "2026-09-15")
(define-obsolete-variable-alias 'night/fim-scope
  'night/llm-scope "2026-09-15")
(define-obsolete-variable-alias 'night/fim-flash-context
  'night/llm-flash-context "2026-09-15")
(define-obsolete-variable-alias 'night/ellama--code-context-before
  'night/llm-context-before "2026-09-15")
(define-obsolete-variable-alias 'night/ellama--code-context-after
  'night/llm-context-after "2026-09-15")
(define-obsolete-variable-alias 'night/ellama--code-context-before-fast
  'night/llm-context-before-fast "2026-09-15")
(define-obsolete-variable-alias 'night/ellama--code-context-after-fast
  'night/llm-context-after-fast "2026-09-15")
(define-obsolete-variable-alias 'night/ellama--code-context-line-tol
  'night/llm-context-line-tol "2026-09-15")

;;;
(defvar night/llm-context-before 10000
  "Characters before point to include as context, for the chat-style commands.")
(defvar night/llm-context-after 10000
  "Characters after point to include as context, for the chat-style commands.")
(defvar night/llm-context-before-fast 1000
  "Characters before point to include as context, for the FIM commands.")
(defvar night/llm-context-after-fast 1000
  "Characters after point to include as context, for the FIM commands.")
(defvar night/llm-context-line-tol 200
  "Budget for rounding a context window out to whole lines.
The window is taken to line boundaries unless doing so would add more
than this many characters to the prompt.")

;;;
(defcustom night/llm-scope 'nearby
  "Region a command may read before sending it to a model, everywhere.

These commands send the text around point to a third-party API.  A scope
narrows what it is allowed to look at:

`nearby'   the text around point, bounded by
           `night/llm-context-before-fast' and its -after- twin.
`block'    the enclosing block: an Org block in `org-mode', the
           enclosing defun anywhere else.
`subtree'  the current heading and its children.  `org-mode' only.

What is sent is always the scope INTERSECTED with the `nearby' window: a
scope narrows, it never buys a bigger budget.

`night/llm--scope-local' overrides this per buffer, and an explicit
`:scope' overrides both for one call.  Nothing writes either of these two
except `night/llm-scope-select' and `night/llm-scope-select-global'."
  :type '(choice (const nearby) (const block) (const subtree))
  :group 'night)

(defvar-local night/llm--scope-local nil
  "Buffer-local override of `night/llm-scope', or nil to inherit it.
Set only by `night/llm-scope-select'.  Never persisted.")

(defcustom night/llm-flash-context t
  "When non-nil, flash the region a completion actually sent.

Shown in the scope's own face, so the colour matches what the chooser
highlighted when you picked it.  Applies in every buffer, not only the
ones `night/llm-path-policy' asks about."
  :type 'boolean
  :group 'night)

;;;
;; Backgrounds rather than foregrounds, because these mark an extent
;; rather than a token, and `:extend' so a multi-line region reads as a
;; block instead of a ragged right edge.
(night/defface night/llm-scope-nearby-face
  '((((background dark))  (:background "#2b2b3b" :extend t))
    (((background light)) (:background "#ecedf7" :extend t)))
  "Face for the `nearby' context scope, the widest of the three.")

(night/defface night/llm-scope-subtree-face
  '((((background dark))  (:background "#343a52" :extend t))
    (((background light)) (:background "#dee4f6" :extend t)))
  "Face for the `subtree' context scope.")

(night/defface night/llm-scope-block-face
  '((((background dark))  (:background "#414c73" :extend t))
    (((background light)) (:background "#ccd7f3" :extend t)))
  "Face for the `block' context scope, the narrowest of the three.")

(defvar night/h-llm-scopes
  '((block   :rank 0 :char ?b :face night/llm-scope-block-face
             :desc "the Org block, or defun, around point")
    (subtree :rank 1 :char ?s :face night/llm-scope-subtree-face
             :desc "the current heading and its children")
    (nearby  :rank 2 :char ?n :face night/llm-scope-nearby-face
             :desc "the text around point, as before"))
  "The context scopes, narrowest first.

:rank orders them by width, which is what lets a confirmation granted for
one scope cover a narrower one without covering a wider one.  :char is
the key in the chooser, :face the highlight, :desc the one-line gloss.")

(defun night/h-llm--scope-get (scope key)
  "Return KEY of SCOPE in `night/h-llm-scopes'."
  (plist-get (alist-get scope night/h-llm-scopes) key))

(defun night/h-llm--scope-rank (scope)
  "Return how wide SCOPE is; bigger is wider.
An unknown scope ranks widest, so that it never passes for consent that
was granted to something narrower."
  (or (night/h-llm--scope-get scope :rank) most-positive-fixnum))

(defun night/h-llm--scope-effective ()
  "Return the context scope in force in the current buffer."
  (or night/llm--scope-local night/llm-scope))

;;;
(defcustom night/llm-path-policy
  '((encrypted                  . refuse)
    ("/\\.keys/"                . refuse)
    ("/\\.privateShell\\Z"      . refuse)
    ("/\\.authinfo(\\.gpg)?\\Z" . refuse)
    ("/\\.netrc\\Z"             . refuse)
    ("/\\.ssh/"                 . refuse)
    ;; macOS resolves /tmp and /var into /private/, so without this the
    ;; rule below would ask about every scratch file.
    ("\\A/private/(tmp|var)/"    . allow)
    ("/notes/private/research/"    . allow)
    ("/private/"                . confirm))
  "What a model-facing command may do in a buffer, most specific rule first.

These commands send buffer text to a third-party API, so some buffers
have no business being completed at all.  Each rule is a cons of a
matcher and a level:

matcher  a PCRE, tested against both the buffer's file name and its
         truename -- see `night/file-path-candidates' -- or a symbol
         naming a predicate in `night/h-llm-policy-predicates'.
level    `refuse' declines and names the rule that said so;
         `confirm' asks once per buffer; `allow' sends.

The FIRST matching rule decides, which is what makes carve-outs
expressible: an allow rule for \"/private/pub/\" placed above the
confirm rule for \"/private/\" exempts that subtree.  That is equally
the hazard -- a broad allow near the top silently disarms everything
under it -- so keep the specific rules on top.  A buffer matching no
rule is allowed, and a buffer visiting no file matches no PCRE rule.

A matcher naming a predicate that does not exist refuses rather than
being skipped: a typo here must not quietly widen the policy."
  :type '(alist :key-type (choice (string :tag "PCRE")
                                  (symbol :tag "Predicate"))
                :value-type (choice (const refuse)
                                    (const confirm)
                                    (const allow)))
  :group 'night)

(defvar night/h-llm-policy-predicates
  '((encrypted . night/buffer-encrypted-p))
  "Symbols usable as matchers in `night/llm-path-policy'.
Each maps to a function of one argument, the buffer to judge.")

(defun night/h-llm--scope-bounds (scope &optional pos)
  "Return (BEG . END) for SCOPE around POS, or nil if none resolves there.

`nearby' resolves to the whole accessible buffer, so that a caller can
intersect unconditionally; the real limit on it is the context window,
which `night/h-llm-code-context-bounds' applies separately.

Bounds that do not contain POS count as no resolution, so that a caller
can never end up widening by accident."
  (let* ((pos (or pos (point)))
         (bounds
          (save-excursion
            (goto-char pos)
            (cond
             ((eq scope 'nearby) (cons (point-min) (point-max)))
             ((eq scope 'block)
              (cond
               ((derived-mode-p 'org-mode)
                ;; Outer, not inner: the `#+begin_src python' line names the
                ;; language and header args, which materially helps the
                ;; completion and is inside the block being approved anyway.
                ;; Nil rather than a defun fallback if the text object is not
                ;; loaded -- `beginning-of-defun' means something unrelated in
                ;; Org, and guessing wide is the one thing a scope must not do.
                (when (fboundp 'night/evil-org-block-textobj--bounds)
                  (night/evil-org-block-textobj--bounds t)))
               (t (bounds-of-thing-at-point 'defun))))
             ((eq scope 'subtree)
              (cond
               ((not (derived-mode-p 'org-mode)) nil)
               ((org-before-first-heading-p) nil)
               (t (cons (save-excursion (org-back-to-heading t) (point))
                        (save-excursion (org-back-to-heading t)
                                        (org-end-of-subtree t t)
                                        (point))))))
             (t nil)))))
    (cond
     ((null bounds) nil)
     ((and (<= (car bounds) pos) (<= pos (cdr bounds))) bounds)
     (t nil))))

(defun night/h-llm--clamp (bounds limit)
  "Intersect BOUNDS with LIMIT, each a cons of buffer positions.
A nil LIMIT clamps nothing."
  (cond
   ((null limit) bounds)
   (t (cons (max (car bounds) (car limit))
            (min (cdr bounds) (cdr limit))))))

(defun night/h-llm--size-string (n)
  "Render N characters compactly enough for a prompt."
  (cond
   ((< n 1000) (format "%dc" n))
   (t (format "%.1fk" (/ n 1000.0)))))

;;;
(defun night/h-llm--overlay-clear (overlay)
  "Delete OVERLAY and drop it from `night/active-overlays'."
  (when (overlayp overlay)
    (delete-overlay overlay)
    (setq night/active-overlays (remove overlay night/active-overlays))))

(defun night/h-llm--preview-make (scope bounds)
  "Highlight BOUNDS in SCOPE's face and return the overlay.

The narrower scope takes the higher priority, so the nested highlights
read innermost-first rather than whichever happened to be drawn last."
  (let ((overlay (make-overlay (car bounds) (cdr bounds))))
    (overlay-put overlay 'face (night/h-llm--scope-get scope :face))
    (overlay-put overlay 'priority (- 100 (night/h-llm--scope-rank scope)))
    ;; Registering here means `night/clear-overlays' (on `doom-escape-hook')
    ;; is a second net under the `unwind-protect' that normally removes it.
    (push overlay night/active-overlays)
    overlay))

(cl-defun night/h-llm--scope-choices (&key (pos nil) (all nil))
  "Return the context scopes at POS, narrowest first.
An alist of (SCOPE . BOUNDS).

BOUNDS is what that scope would actually send -- the scope intersected
with the context window -- or nil where the scope does not resolve here.

Scopes that do not resolve are dropped unless ALL, which the selectors
want: you may well be setting `subtree' from a spot that has none yet."
  (let* ((pos (or pos (point)))
         (window (night/h-llm-code-context-bounds pos)))
    (delq nil
          (mapcar
           (lambda (entry)
             (let* ((scope (car entry))
                    (bounds (night/h-llm--scope-bounds scope pos)))
               (cond
                (bounds (cons scope (night/h-llm--clamp window bounds)))
                (all (cons scope nil))
                (t nil))))
           night/h-llm-scopes))))

(cl-defun night/h-llm--scope-choose (&key (prompt "Send") (pos nil)
                                          (all nil) (cancel t))
  "Ask which context scope to use, highlighting every candidate while asking.

All the candidates are highlighted at once rather than previewed one at a
time.  They nest -- block within subtree within window -- so a single
rendering answers all three questions, and the choice then costs one
keypress instead of a walk through a list.

Sizes go in the choice names because a subtree is routinely taller than
the window, and the highlight alone would quietly under-report what is
about to be sent.

Return the chosen scope, or nil if cancelled."
  (let* ((pos (or pos (point)))
         (choices (night/h-llm--scope-choices :pos pos :all all))
         (overlays nil))
    (cond
     ((null choices) nil)
     (t
      (unwind-protect
          (let ((table
                 (append
                  (mapcar
                   (lambda (choice)
                     (let ((scope (car choice))
                           (bounds (cdr choice)))
                       (list (night/h-llm--scope-get scope :char)
                             (cond
                              (bounds
                               (format "%s %s" scope
                                       (night/h-llm--size-string
                                        (- (cdr bounds) (car bounds)))))
                              (t (format "%s (none here)" scope)))
                             (night/h-llm--scope-get scope :desc))))
                   choices)
                  (when cancel
                    (list (list ?c "cancel" "send nothing"))))))
            (dolist (choice choices)
              (when (cdr choice)
                (push (night/h-llm--preview-make (car choice) (cdr choice))
                      overlays)))
            (let ((answer (car (read-multiple-choice prompt table))))
              (car (cl-find-if
                    (lambda (choice)
                      (eq (night/h-llm--scope-get (car choice) :char) answer))
                    choices))))
        (mapc #'night/h-llm--overlay-clear overlays))))))

;;;
(defvar-local night/llm--path-confirmed nil
  "Scope a `confirm' rule was approved at in this buffer, or nil.

The scope rather than a bare t, so that approving `block' does not
silently become approval for `nearby' on the next keystroke.  A later
call is covered while its scope is no wider than the one approved; a
wider one asks again.

Buffer-local and never persisted, so revisiting the file asks again.")

(defun night/h-llm--confirmed-p (scope)
  "Non-nil if SCOPE is covered by this buffer's stored confirmation."
  (and night/llm--path-confirmed
       (<= (night/h-llm--scope-rank scope)
           (night/h-llm--scope-rank night/llm--path-confirmed))))
(defun night/h-llm--policy-rule-match-p (rule paths buffer)
  "Non-nil if RULE of `night/llm-path-policy' applies to BUFFER.

PATHS is what `night/file-path-candidates' returned for it.  A symbol
matcher with no entry in `night/h-llm-policy-predicates' counts as a
match, so that `night/h-llm--gate' is the one that gets to refuse on it."
  (let ((matcher (car rule)))
    (if (stringp matcher)
        (let ((regexp (night/pcre-to-regexp matcher)))
          ;; A pattern that will not compile counts as a match, so that
          ;; `night/h-llm--gate' refuses on it instead of walking past.
          (or (null regexp)
              (cl-some (lambda (path) (string-match-p regexp path)) paths)))
      (let ((fn (alist-get matcher night/h-llm-policy-predicates)))
        (if fn (funcall fn buffer) t)))))

(cl-defun night/h-llm--gate (&key (buffer nil) (scope nil))
  "Decide whether a model-facing command may run in BUFFER, per
`night/llm-path-policy'.

SCOPE, when non-nil, is a scope the caller asked for explicitly.  Naming
what you send is consent enough for a `confirm' rule, so such a call is
never prompted -- a `refuse' rule still refuses, as it refuses everything.
With SCOPE nil the buffer's effective scope is used, and a `confirm' rule
raises the chooser.

Return a cons of an outcome and a value:

(ok    . SCOPE)   go ahead, reading no more than SCOPE
(info  . REASON)  declined; report quietly
(error . REASON)  refused; report as a failure

This may prompt.  A `confirm' answered once is remembered for as long as
the buffer lives, but only for scopes no wider than the one approved --
see `night/llm--path-confirmed'."
  (let* ((buffer (or buffer (current-buffer)))
         (case-fold-search nil)
         (explicit scope)
         (wanted (or scope
                     (with-current-buffer buffer
                       (night/h-llm--scope-effective))))
         (paths (night/file-path-candidates (buffer-file-name buffer)))
         (rule (cl-find-if
                (lambda (rule)
                  (night/h-llm--policy-rule-match-p rule paths buffer))
                night/llm-path-policy)))
    (cond
     ((null rule) (cons 'ok wanted))
     (t
      (let ((matcher (car rule))
            (level (cdr rule)))
        (cond
         ((and (stringp matcher) (null (night/pcre-to-regexp matcher)))
          (cons 'error (format "cannot read `%s' as a PCRE; refusing" matcher)))
         ((and (not (stringp matcher))
               (not (alist-get matcher night/h-llm-policy-predicates)))
          (cons 'error (format "`%s' names no predicate; refusing" matcher)))
         ((eq level 'allow) (cons 'ok wanted))
         ((eq level 'refuse)
          (cons 'error (format "refused by `%s'" matcher)))
         ((eq level 'confirm)
          (with-current-buffer buffer
            (cond
             (explicit (cons 'ok wanted))
             ((night/h-llm--confirmed-p wanted) (cons 'ok wanted))
             (t
              (let ((chosen
                     (night/h-llm--scope-choose
                      :prompt (format "%s matches `%s'; send"
                                      (buffer-name buffer) matcher))))
                (cond
                 ((null chosen) (cons 'info "declined"))
                 (t
                  ;; Records consent, not preference: this never feeds
                  ;; `night/h-llm--scope-effective'.
                  (setq night/llm--path-confirmed chosen)
                  (cons 'ok chosen))))))))
         (t
          ;; Fail closed on a level nobody defined.
          (cons 'error (format "unknown level `%s' in `%s'" level matcher)))))))))
;;;
(defun night/llm-scope-show ()
  "Echo the context scope in force here, and where it comes from."
  (interactive)
  (message "LLM context scope: %s (buffer: %s, global: %s)"
           (night/h-llm--scope-effective)
           (or night/llm--scope-local "inherit")
           night/llm-scope))

(defun night/llm-scope-select (scope)
  "Make SCOPE the context scope for this buffer, overriding `night/llm-scope'."
  (interactive
   (list (night/h-llm--scope-choose :prompt "Scope in this buffer:"
                                    :all t :cancel nil)))
  (when scope
    (setq night/llm--scope-local scope)
    (night/llm-scope-show)))

(defun night/llm-scope-select-global (scope)
  "Make SCOPE the default context scope everywhere.
Buffers with their own `night/llm--scope-local' keep it."
  (interactive
   (list (night/h-llm--scope-choose :prompt "Scope everywhere:"
                                    :all t :cancel nil)))
  (when scope
    (setq night/llm-scope scope)
    (night/llm-scope-show)))

(defun night/h-llm-code-context-bounds (point)
  "Return (prefix-start . suffix-end) for the context around POINT."
  (let* ((before-point (max (point-min) (- point night/llm-context-before-fast)))
         (after-point (min (point-max) (+ point night/llm-context-after-fast)))
         (start-of-line-before (save-excursion
                                 (goto-char before-point)
                                 (forward-line 0)
                                 (point)))
         (end-of-line-after (save-excursion
                              (goto-char after-point)
                              (end-of-line)
                              (point)))
         (prefix-start (if (> (- start-of-line-before before-point) night/llm-context-line-tol)
                           before-point
                         start-of-line-before))
         (suffix-end (if (> (- after-point end-of-line-after) night/llm-context-line-tol)
                         after-point
                       end-of-line-after)))
    (cons prefix-start suffix-end)))
;;;
;; These were all spelled `night/fim-*' while this machinery served only the
;; FIM commands.  It governs every model-facing command now, so the name would
;; have been a lie in any backtrace out of an ellama command.
;;
;; The variable aliases are up top rather than here: declared after their
;; referent, the new `defvar' wins and a value customized under the old name
;; is silently dropped, which is the one thing these aliases exist to stop.
(define-obsolete-function-alias 'night/fim-scope-show
  #'night/llm-scope-show "2026-09-15")
(define-obsolete-function-alias 'night/fim-scope-select
  #'night/llm-scope-select "2026-09-15")
(define-obsolete-function-alias 'night/fim-scope-select-global
  #'night/llm-scope-select-global "2026-09-15")

;; A face takes an alias rather than an obsolescence declaration; same shape as
;; the `at-tag-face' alias in night-ui.el.
(put 'night/fim-scope-nearby-face  'face-alias 'night/llm-scope-nearby-face)
(put 'night/fim-scope-subtree-face 'face-alias 'night/llm-scope-subtree-face)
(put 'night/fim-scope-block-face   'face-alias 'night/llm-scope-block-face)

;;;
(provide 'night-llm-context)

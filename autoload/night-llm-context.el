;;; autoload/night-llm-context.el -*- lexical-binding: t; -*-
;;
;; What a command may read out of a buffer before sending it to a model, and
;; whether it may send at all.
;;
;; Shared by `night/llm-fim-insert-at-point' in night-llm-fim.el and by the
;; ellama commands in night-ellama.el.  It lives in its own file because those
;; two cannot depend on each other: night-llm-fim.el is wrapped in
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
(declare-function night/flash-region "night-ui")
(declare-function outline-back-to-heading "outline")
(declare-function outline-next-heading "outline")

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
`subtree'  the current heading and its children, in `org-mode' and
           `markdown-mode'.

What is sent is always the scope INTERSECTED with the `nearby' window: a
scope narrows, it never buys a bigger budget.

`night/llm-scope-file' narrows this from the file itself,
`night/llm--scope-local' overrides it per buffer, and an explicit
`:scope' overrides all of them for one call.  Nothing writes either of
the latter two except `night/llm-scope-select' and
`night/llm-scope-select-global'."
  :type '(choice (const nearby) (const block) (const subtree))
  :group 'night)

(defvar-local night/llm--scope-local nil
  "Buffer-local override of `night/llm-scope', or nil to inherit it.
Set only by `night/llm-scope-select'.  Never persisted.")

(defvar-local night/llm-scope-file nil
  "A scope the file asks for, set as a file-local variable.

    ;; -*- night/llm-scope-file: block -*-

or through a `Local Variables' block; `add-file-local-variable' writes
either for you.  It applies to every mode, unlike anything keyed on Org
or markdown structure.

HONOURED ONLY WHEN IT NARROWS.  This is a mechanism for keeping text off
the wire, and a file-local lets the very content being protected say how
much of itself may be sent -- a repository you cloned, or a note someone
sent you, could ask for `buffer'.  So a request is obeyed when its rank
is at or below `night/llm-scope', and ignored otherwise.  Narrowing can
only reduce what leaves the machine; at worst the scope fails to resolve
and the command refuses, which is a nuisance and not a leak.

That guard is also what makes this safe as a local variable with no
prompt, so it carries a `safe-local-variable' predicate: any known scope
is a safe value, because a widening one is discarded rather than obeyed.

Setting this also answers a `confirm' rule in `night/llm-path-policy',
the same way naming a scope at the keystroke does: it authorises that
one file's own text, and no more of it than `night/llm-scope' already
allowed.  `refuse' still refuses.

An explicit `night/llm-scope-select' (`leader . o') outranks this: a
keystroke you just pressed is always the last word over a line in a
file, and it also takes the consent back, so the `confirm' rule asks
again.  `night/llm-scope-show' says which of the two is in force, and
says so when a request was discarded for widening.")

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
;;
;; These were three steps of one blue gradient, on the reasoning that the
;; scopes nest so their colours should too.  They were not tellable apart:
;; the steps differed almost only in lightness, and the nesting is already
;; obvious from the shapes, one region sitting inside another.
;;
;; The replacement separates them by HUE as well, on the red-blue axis
;; rather than the blue-green one -- `modus-operandi-tritanopia' is in use
;; here, and blue-versus-green is exactly what a tritanopia palette cannot
;; lean on, as is yellow.  So: neutral for the widest, red for the middle,
;; blue for the narrowest, with wide lightness steps as a second cue for
;; anyone the hues do not reach.  Red for `buffer' too, deeper, since it
;; never shares a rendering with the other three.
(night/defface night/llm-scope-nearby-face
  '((((background dark))  (:background "#2f2f36" :extend t))
    (((background light)) (:background "#eaeaee" :extend t)))
  "Face for the `nearby' context scope, the widest of the three.
Neutral: it is the backdrop the narrower ones sit on.")

(night/defface night/llm-scope-subtree-face
  '((((background dark))  (:background "#5a2a2a" :extend t))
    (((background light)) (:background "#f8cfcf" :extend t)))
  "Face for the `subtree' context scope.  Red, against `block''s blue.")

(night/defface night/llm-scope-block-face
  '((((background dark))  (:background "#25406e" :extend t))
    (((background light)) (:background "#c2d6f8" :extend t)))
  "Face for the `block' context scope, the narrowest of the three.
Blue, and the most saturated of the three: it is the innermost region
and usually the one being sent.")

(night/defface night/llm-scope-buffer-face
  '((((background dark))  (:background "#6e2f2f" :extend t))
    (((background light)) (:background "#f0b9b9" :extend t)))
  "Face for the `buffer' context scope.
A deeper red than `subtree''s, and never rendered beside it: `buffer' is
offered only to Copilot, which can do nothing narrower.  It was warm
yellow, which is the one hue a tritanopia palette cannot use.")

(defvar night/h-llm-scopes
  '((block   :rank 0 :char ?b :windowed t :face night/llm-scope-block-face
             :desc "the Org block, or defun, around point")
    (subtree :rank 1 :char ?s :windowed t :face night/llm-scope-subtree-face
             :desc "the current heading and its children")
    (nearby  :rank 2 :char ?n :windowed t :face night/llm-scope-nearby-face
             :desc "the text around point, as before")
    (buffer  :rank 3 :char ?w :windowed nil :face night/llm-scope-buffer-face
             :desc "the whole buffer, which is what Copilot syncs"))
  "The context scopes, narrowest first.

:rank orders them by width, which is what lets a confirmation granted for
one scope cover a narrower one without covering a wider one.  :char is
the key in the chooser, :face the highlight, :desc the one-line gloss.

:windowed says whether the scope is also bounded by the context window.
The first three are -- they narrow a window that was already capped.
`buffer' is not: Copilot syncs the whole file, and intersecting that with
the +-1000 window would show a reassuring lie.")

;; Down here rather than beside the `defvar-local', because the predicate
;; reads `night/h-llm-scopes' and that table is defined just above.  It only
;; ever runs when a file is opened, so the original placement worked -- but it
;; compiled to a free-variable warning, and a load-order accident away from
;; being a real bug.
(put 'night/llm-scope-file 'safe-local-variable
     (lambda (v) (and (symbolp v) (assq v night/h-llm-scopes))))

(defvar night/h-llm-scopes-default '(block subtree nearby)
  "Scopes offered when a caller does not say which it can honour.
`buffer' is left out: no command that reads a window can promise it.")

(defun night/h-llm--scope-get (scope key)
  "Return KEY of SCOPE in `night/h-llm-scopes'."
  (plist-get (alist-get scope night/h-llm-scopes) key))

(defun night/h-llm--scope-rank (scope)
  "Return how wide SCOPE is; bigger is wider.
An unknown scope ranks widest, so that it never passes for consent that
was granted to something narrower."
  (or (night/h-llm--scope-get scope :rank) most-positive-fixnum))

(defun night/h-llm--scope-widest (scopes)
  "Return the widest of SCOPES by `night/h-llm--scope-rank'."
  (car (last (sort (copy-sequence scopes)
                   (lambda (a b) (< (night/h-llm--scope-rank a)
                                    (night/h-llm--scope-rank b)))))))

(defun night/h-llm--scope-file ()
  "Return the scope the file asked for, when it narrows; nil otherwise.

See `night/llm-scope-file' for why only narrowing is honoured.  An
unknown scope ranks widest, so a typo is discarded here rather than
taken for something narrower."
  (let ((want night/llm-scope-file))
    (when (and want
               (assq want night/h-llm-scopes)
               (<= (night/h-llm--scope-rank want)
                   (night/h-llm--scope-rank night/llm-scope)))
      want)))

(defun night/h-llm--scope-effective ()
  "Return the context scope in force in the current buffer."
  (or night/llm--scope-local
      (night/h-llm--scope-file)
      night/llm-scope))

;;;
(defun night/h-llm--say (fn label face fmt &rest args)
  "Report FMT and ARGS about a refusal.

FN, when given, is called like `message' and owns the formatting -- that
is how FIM keeps its own \"FIM: \" prefix and its verbosity flag.
Otherwise the text is prefixed with LABEL and shown in FACE."
  (cond
   (fn (apply fn fmt args))
   (t (let ((text (format "%s: %s" label (apply #'format fmt args))))
        (message "%s" (cond
                       (face (propertize text 'face face))
                       (t text)))))))

(cl-defun night/h-llm--gate-scope (&key (scope nil) (buffer nil) (label "LLM")
                                        (report nil) (report-error nil)
                                        (scopes nil))
  "Return the scope a command may read in BUFFER, or nil if it may not.

Wraps `night/h-llm--gate' and reports the refusal itself, so that every
caller is a single nil check rather than its own copy of the three-way
verdict.  SCOPE, when non-nil, is an explicitly requested scope.  REPORT
and REPORT-ERROR are passed to `night/h-llm--say'."
  (let* ((verdict (night/h-llm--gate :buffer buffer :scope scope :scopes scopes))
         (outcome (car verdict)))
    (cond
     ((eq outcome 'ok) (cdr verdict))
     ((eq outcome 'info)
      (night/h-llm--say report label nil "%s" (cdr verdict))
      nil)
     (t
      (night/h-llm--say report-error label 'error "%s" (cdr verdict))
      nil))))

(cl-defun night/h-llm--narrow (bounds &key (scope nil) (pos nil) (label "LLM")
                                           (report-error nil))
  "Return BOUNDS narrowed to SCOPE at POS, or nil after saying why not.

BOUNDS is the caller's own window as (BEG . END).  A scope that does not
resolve here refuses, rather than leaving BOUNDS unclamped: widening in
silence is the one failure this whole mechanism exists to prevent."
  (let ((limit (night/h-llm--scope-bounds scope pos)))
    (cond
     ((null limit)
      (night/h-llm--say report-error label 'error
                        "no `%s' here; not widening" scope)
      nil)
     (t (night/h-llm--clamp bounds limit)))))

(defun night/h-llm--flash (bounds scope)
  "Flash BOUNDS in SCOPE's face, if `night/llm-flash-context' says to."
  (when (and night/llm-flash-context bounds)
    (night/flash-region (car bounds) (cdr bounds)
                        :face (night/h-llm--scope-get scope :face))))

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
which `night/llm-context-bounds' applies separately.

Bounds that do not contain POS count as no resolution, so that a caller
can never end up widening by accident."
  (let* ((pos (or pos (point)))
         (bounds
          (save-excursion
            (goto-char pos)
            (cond
             ((eq scope 'nearby) (cons (point-min) (point-max)))
             ;; Widened, because `copilot--get-source' widens: a narrowed
             ;; buffer is not protected, so it must not be reported as if it
             ;; were.
             ((eq scope 'buffer)
              (save-restriction (widen) (cons (point-min) (point-max))))
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
               ((derived-mode-p 'markdown-mode)
                ;; A fenced code block, outer again -- the ```python line is
                ;; the same kind of hint as Org's `#+begin_src' header.
                ;;
                ;; No defun fallback here either.  Prose is the common case in
                ;; a markdown buffer, `bounds-of-thing-at-point' would hand
                ;; back a paragraph-ish region with no relationship to what the
                ;; user approved, and point outside every fence must simply
                ;; refuse.
                ;;
                ;; `markdown-get-enclosing-fenced-block-construct' reads the
                ;; syntax properties markdown-mode propertizes lazily.  It
                ;; resolved without forcing in every case probed, including a
                ;; fresh temp buffer; `syntax-propertize' is insurance for a
                ;; large buffer whose propertization has not reached point yet,
                ;; and costs nothing where it already has.  `font-lock-ensure'
                ;; would also work and is far more expensive.
                (when (fboundp 'markdown-get-enclosing-fenced-block-construct)
                  (syntax-propertize (point))
                  (let ((fence (markdown-get-enclosing-fenced-block-construct)))
                    (when fence (cons (car fence) (cadr fence))))))
               (t (bounds-of-thing-at-point 'defun))))
             ((eq scope 'subtree)
              (cond
               ((derived-mode-p 'org-mode)
                (if (org-before-first-heading-p)
                    nil
                  (cons (save-excursion (org-back-to-heading t) (point))
                        (save-excursion (org-back-to-heading t)
                                        (org-end-of-subtree t t)
                                        (point)))))
               ((derived-mode-p 'markdown-mode)
                ;; Walk forward to the next heading of level <= this one, so
                ;; children are included.  `markdown-outline-next' stops at the
                ;; next heading of *any* level and would cut them off -- the
                ;; same mistake `night/org-heading-region-get' makes, which is
                ;; why the Org arm above does not use it either.
                ;;
                ;; `outline-back-to-heading' signals before the first heading
                ;; rather than returning nil, and that is the ordinary case in
                ;; a markdown file that opens with prose, so it is caught
                ;; rather than pre-tested.
                (when (ignore-errors (outline-back-to-heading t) t)
                  (let ((beg (point))
                        (level (funcall outline-level)))
                    (outline-next-heading)
                    (while (and (not (eobp))
                                (> (funcall outline-level) level))
                      (outline-next-heading))
                    (cons beg (point)))))
               (t nil)))
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
  (let ((overlay (save-restriction
                   ;; `buffer' bounds are widened ones, which `make-overlay'
                   ;; will not accept while the buffer is narrowed.
                   (widen)
                   (make-overlay (car bounds) (cdr bounds)))))
    (overlay-put overlay 'face (night/h-llm--scope-get scope :face))
    (overlay-put overlay 'priority (- 100 (night/h-llm--scope-rank scope)))
    ;; Registering here means `night/clear-overlays' (on `doom-escape-hook')
    ;; is a second net under the `unwind-protect' that normally removes it.
    (push overlay night/active-overlays)
    overlay))

(cl-defun night/h-llm--scope-choices (&key (pos nil) (all nil) (scopes nil))
  "Return the context scopes at POS, narrowest first.
An alist of (SCOPE . BOUNDS).

BOUNDS is what that scope would actually send -- the scope intersected
with the context window -- or nil where the scope does not resolve here.

Scopes that do not resolve are dropped unless ALL, which the selectors
want: you may well be setting `subtree' from a spot that has none yet.

SCOPES limits which ones are offered, for a caller that cannot honour all
of them; it defaults to `night/h-llm-scopes-default'.  Only `:windowed'
scopes are intersected with the context window."
  (let* ((pos (or pos (point)))
         (scopes (or scopes night/h-llm-scopes-default))
         (window (night/llm-context-bounds :pos pos)))
    (delq nil
          (mapcar
           (lambda (entry)
             (let* ((scope (car entry))
                    (bounds (night/h-llm--scope-bounds scope pos)))
               (cond
                ((not (memq scope scopes)) nil)
                (bounds
                 (cons scope
                       (cond
                        ((night/h-llm--scope-get scope :windowed)
                         (night/h-llm--clamp window bounds))
                        (t bounds))))
                (all (cons scope nil))
                (t nil))))
           night/h-llm-scopes))))

(defun night/h-llm--read-key (prompt table)
  "Read one key from TABLE, which is `read-multiple-choice\' shaped.

TABLE entries are (CHAR NAME DESCRIPTION).  Returns the chosen char, or
nil when cancelled.  `?\' lists the descriptions and asks again.

This is `read-char-from-minibuffer\' rather than `read-multiple-choice\',
for one reason: it reads in the MINIBUFFER, and this configuration
already binds ESC there to `abort-recursive-edit\' -- check it with

    (lookup-key read-char-from-minibuffer-map [escape])

so ESC and C-g both cancel for free, with nothing added to the table and
nothing extra printed in the prompt.  `read-multiple-choice\' accepts
only keys that are in its table and builds its prompt from that same
table, so ESC there had to be either broken or listed redundantly beside
`cancel\'.

`read-answer\' sits on the same reader and would also get ESC, but in
short mode it prints only the keys -- \"Send (b, s, n, c, ?)\" -- which
drops the sizes, and those are the point: a subtree is routinely taller
than the window and the highlight alone under-reports what is about to
leave the machine."
  (let ((chars (append (mapcar #'car table) (list ??)))
        (line (mapconcat (lambda (e)
                           (format "[%c]%s" (car e) (cadr e)))
                         table ", ")))
    (condition-case nil
        (let (answer)
          (while (null answer)
            (let ((key (read-char-from-minibuffer
                        (format "%s (%s, [?]help): " prompt line) chars)))
              (cond
               ((eq key ??)
                (message "%s" (mapconcat (lambda (e)
                                           (format "%c = %s" (car e) (or (nth 2 e) (cadr e))))
                                         table "; ")))
               (t (setq answer key)))))
          answer)
      ;; ESC and C-g arrive here; the caller's `unwind-protect' clears the
      ;; overlays, and nil is already what cancelled means.
      (quit nil))))

(cl-defun night/h-llm--scope-choose (&key (prompt "Send") (pos nil)
                                          (all nil) (cancel t) (scopes nil))
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
         (choices (night/h-llm--scope-choices :pos pos :all all :scopes scopes))
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
            (let ((answer (night/h-llm--read-key prompt table)))
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

(cl-defun night/h-llm--gate (&key (buffer nil) (scope nil) (scopes nil))
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
         (scopes (or scopes night/h-llm-scopes-default))
         (wanted (or scope
                     (let ((eff (with-current-buffer buffer
                                  (night/h-llm--scope-effective))))
                       (cond
                        ((memq eff scopes) eff)
                        ;; A caller that cannot honour the buffer's scope is
                        ;; judged on the widest thing it *can* do.  Without
                        ;; this, Copilot -- which only does `buffer' -- would
                        ;; be waved through by a confirmation granted to FIM
                        ;; for a 1000-char window.
                        (t (night/h-llm--scope-widest scopes))))))
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
             ;; A scope the file asked for is consent, on the same footing as
             ;; naming one at the keystroke: what it authorises is that one
             ;; file's own text, and no more of it than `night/llm-scope'
             ;; already allowed, because `night/h-llm--scope-file' honours a
             ;; request only when it narrows.  A hostile `-*-' line would be
             ;; publishing its author's own content, which buys nothing.
             ;;
             ;; The residual case, accepted knowingly: a file you did not write
             ;; that carries the line, to which you later add something
             ;; sensitive.
             ;;
             ;; Only while the file is what actually governs -- a
             ;; `night/llm-scope-select' override means the buffer is speaking,
             ;; not the file, and then the prompt is owed again.  And only for
             ;; `confirm'; `refuse' refuses everything, this included.
             ((and (null night/llm--scope-local)
                   (night/h-llm--scope-file))
              (cons 'ok wanted))
             ((night/h-llm--confirmed-p wanted) (cons 'ok wanted))
             (t
              (let ((chosen
                     (night/h-llm--scope-choose
                      :scopes scopes
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
(defcustom night/llm-scope-show-seconds 1
  "Seconds `night/llm-scope-show\' highlights the scopes at point, or nil.

Every scope that resolves here is painted at once, in the same faces the
chooser uses, so a glance answers \"what would each of these actually
send from where I am standing?\" -- which the echoed name alone cannot.
They nest, so one rendering shows all of them."
  :type '(choice (const :tag "do not highlight" nil) number)
  :group 'night)

(defvar-local night/h-llm--show-overlays nil
  "Overlays `night/llm-scope-show\' is currently showing.")

(defvar-local night/h-llm--show-timer nil
  "Timer that will remove `night/h-llm--show-overlays\'.")

(defun night/h-llm--show-clear ()
  "Remove the highlights `night/llm-scope-show\' put up, and its timer."
  (when (timerp night/h-llm--show-timer)
    (cancel-timer night/h-llm--show-timer))
  (setq night/h-llm--show-timer nil)
  (mapc #'night/h-llm--overlay-clear night/h-llm--show-overlays)
  (setq night/h-llm--show-overlays nil))

(defun night/h-llm--show-flash (pos seconds)
  "Paint every scope that resolves at POS, and clear it after SECONDS.

Clears any previous set first, so pressing the key twice cannot leave a
buffer painted twice over or strand the older timer.  The overlays go
through `night/h-llm--preview-make\', so they register in
`night/active-overlays\' and `C-g\' takes them down early."
  (night/h-llm--show-clear)
  (let ((buffer (current-buffer)))
    (dolist (choice (night/h-llm--scope-choices :pos pos))
      (when (cdr choice)
        (push (night/h-llm--preview-make (car choice) (cdr choice))
              night/h-llm--show-overlays)))
    (when night/h-llm--show-overlays
      (setq night/h-llm--show-timer
            (run-at-time seconds nil
                         (lambda ()
                           ;; The buffer may be gone, and the clear writes
                           ;; buffer-local state, so it has to run there.
                           (when (buffer-live-p buffer)
                             (with-current-buffer buffer
                               (night/h-llm--show-clear)))))))))

(defun night/llm-scope-show ()
  "Echo the context scope in force here, and where it comes from.

The effective scope is said once, then where it came from.  Naming it twice --
`nearby (buffer: inherit, global: nearby)\' -- made the common case the noisy
one, and spelled \"no buffer override\" as `inherit\', which reads like a
fourth scope rather than the absence of a setting."
  (interactive)
  (when night/llm-scope-show-seconds
    (night/h-llm--show-flash (point) night/llm-scope-show-seconds))
  (let ((effective (night/h-llm--scope-effective))
        (from-file (night/h-llm--scope-file)))
    (cond
     (night/llm--scope-local
      (message "LLM scope: %s — this buffer (global: %s)"
               effective night/llm-scope))
     (from-file
      (message "LLM scope: %s — this file (global: %s)"
               effective night/llm-scope))
     ;; A request that was thrown away for widening has to be said out loud,
     ;; or the file-local looks like it simply did not work.
     (night/llm-scope-file
      (message "LLM scope: %s — global; this file asked for %s, ignored (only narrowing is honoured)"
               effective night/llm-scope-file))
     (t
      (message "LLM scope: %s — global, no buffer override" effective)))))

(defun night/llm-scope-select (scope)
  "Make SCOPE the context scope for this buffer, overriding `night/llm-scope'.

Cancelling -- `c', ESC or C-g -- leaves the buffer's scope exactly as it
was.  That was always true of the body, which does nothing unless a scope
comes back; what was missing was a way to say so from the prompt."
  (interactive
   (list (night/h-llm--scope-choose :prompt "Scope in this buffer:"
                                    :all t)))
  (when scope
    (setq night/llm--scope-local scope)
    (night/llm-scope-show)))

(defun night/llm-scope-select-global (scope)
  "Make SCOPE the default context scope everywhere.
Buffers with their own `night/llm--scope-local' keep it.

Cancelling leaves the global scope alone, as for
`night/llm-scope-select'."
  (interactive
   (list (night/h-llm--scope-choose :prompt "Scope everywhere:"
                                    :all t)))
  (when scope
    (setq night/llm-scope scope)
    (night/llm-scope-show)))

(defun night/h-llm--snap (pos boundary-fn tol)
  "Round POS out to the line boundary BOUNDARY-FN gives, if that is cheap.

BOUNDARY-FN is `line-beginning-position' or `line-end-position'.  The
cost is how many characters rounding adds to the window; above TOL, POS
is left where it is.

The comparison used to be written the other way round, subtracting the
boundary from POS for the start of the window, where the difference is
never positive.  The test could not fire, so the window always rounded
out no matter how long the line was -- the opposite of what TOL is for."
  (let ((snapped (save-excursion (goto-char pos) (funcall boundary-fn))))
    (cond
     ((<= (abs (- snapped pos)) tol) snapped)
     (t pos))))

(cl-defun night/llm-context-bounds (&key (pos nil) (before nil) (after nil)
                                         (line-tol nil))
  "Return (BEG . END) for the context window around POS.

BEFORE and AFTER are character budgets either side of POS, defaulting to
`night/llm-context-before-fast' and `night/llm-context-after-fast'.  A
budget of 0 pins that side to POS exactly, which is what the commands
that send only a prefix want -- rounding out there would reach past point
and hand the model the answer.

The window is rounded out to whole lines when that costs at most LINE-TOL
characters (`night/llm-context-line-tol'), so a model is not handed half
a token, and one very long line cannot drag in far more than was asked
for.

Scope narrowing is not done here; see `night/h-llm--narrow', which also
reports when a scope fails to resolve."
  (let* ((pos (or pos (point)))
         (before (or before night/llm-context-before-fast))
         (after (or after night/llm-context-after-fast))
         (line-tol (or line-tol night/llm-context-line-tol))
         (raw-beg (max (point-min) (- pos before)))
         (raw-end (min (point-max) (+ pos after))))
    (cons
     (cond
      ((> before 0) (night/h-llm--snap raw-beg #'line-beginning-position line-tol))
      (t raw-beg))
     (cond
      ((> after 0) (night/h-llm--snap raw-end #'line-end-position line-tol))
      (t raw-end)))))
;;;
;; This machinery was spelled `night/llm-fim-*' while it served only the FIM
;; commands; it governs every model-facing command now, so it is `night/llm-*'.
;; The obsolete aliases that carried the old names are gone: they were a day
;; old, had no consumers outside this directory, and once the FIM commands
;; themselves became `night/llm-fim-*' they would have been shims pointing at
;; shims.
;;
;; If an old name ever turns up in a stale byte-compiled file or a saved
;; customization, it will now error rather than resolve quietly, which is the
;; behaviour wanted here.
(provide 'night-llm-context)

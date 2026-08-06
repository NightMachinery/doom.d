;;; autoload/org/night-latex-preview-lazy.el -*- lexical-binding: t; -*-
;;;
;;; Progressive, non-blocking LaTeX previews for org buffers with many
;;; fragments. Stock `org-latex-preview' compiles every fragment
;;; synchronously and can freeze Emacs for minutes on large files;
;;; `night/org-latex-preview-lazy' previews the visible fragments first and
;;; drains the rest in small chunks from idle timers.
;;;
;;; See docs/org/latex-preview/performance.md for the strategy discussion.
;;; This code targets the OLD (pre-9.8) synchronous preview system and
;;; should be retired once the async `org-latex-preview' overhaul lands.

(defun night/org-latex-preview-new-system-p ()
  "Whether the new async `org-latex-preview' system is installed.

The tecosaur/karthink overhaul (expected in org 9.8+) ships a dedicated
org-latex-preview.el library; stock org <= 9.7 defines
`org-latex-preview' as a function inside org.el instead."
  (and (locate-library "org-latex-preview") t))

(when (night/org-latex-preview-new-system-p)
  (display-warning
   'night-org
   "The new async org-latex-preview system has landed! The lazy-preview code in autoload/org/night-latex-preview-lazy.el targets the old synchronous system and needs updating; the new system previews asynchronously natively. See docs/org/latex-preview/performance.md."))

;;;
(defvar night/org-latex-preview-lazy-tick-seconds 0.5
  "Wall-clock work budget per tick.
Bounds how long each tick may block Emacs. Cache-hit fragments render in
about a millisecond (no LaTeX runs), so a warm-cache buffer drains
almost immediately — hundreds of fragments per tick; a cold compile
(~0.3-1s) naturally caps a tick at roughly one fragment.")

(defvar night/org-latex-preview-lazy-idle-delay 0.5
  "Idle seconds the user must be before compiling starts (or resumes).")

(defvar night/org-latex-preview-lazy-rest-delay 0.3
  "Wall-clock seconds to rest between chunks while draining.
This gap lets the event loop process pending input, process output, and
server requests between chunks; without it, back-to-back chunks starve
I/O and Emacs appears frozen despite the timers.")

(defvar night/org-latex-preview-lazy-sync-threshold 1
  "Regions with at most this many fragments compile synchronously.
Used by `night/h-olpl-around-org--latex-preview-region'. The threshold
counts fragments, but the cost driver is cold compiles (~0.3-1s each
versus ~1ms for cache hits), which are unknowable at dispatch time —
so the default of 1 bounds the worst-case synchronous freeze to a
single LaTeX run while keeping the fragment-at-point toggle and
org-fragtog's exit re-renders instant.")

(defvar night/org-latex-preview-lazy-sync-cached-max 5000
  "Render all-cached regions synchronously up to this many fragments.
Warm renders cost ~0.25ms per fragment, so all-cached regions can skip
the lazy queue and appear immediately; the cap bounds both that bulk
render and the hash-checking itself (~10-30µs per fragment) on
pathological files. Checked against the fragment count BEFORE any
hashes are computed.")

(defvar night/h-olpl-inhibit-reroute nil
  "Bound non-nil by the lazy drain so its own
`org--latex-preview-region' calls reach the real function instead of
being rerouted back into the queue.")

(defvar night/org-latex-preview-lazy-enabled-p t
  "Whether the lazy preview machinery is active.
When nil, the `org--latex-preview-region' and `insert-for-yank' advices
pass straight through to stock synchronous behavior — an emergency kill
switch for working around bugs. Toggle per buffer with
`night/org-latex-preview-lazy-toggle' (via `setq-local') or everywhere
with `night/org-latex-preview-lazy-global-toggle'.")

(defvar-local night/h-olpl-queue nil
  "Pending fragments, a list of (BEGIN-MARKER . END-MARKER) conses.")

(defvar-local night/h-olpl-timer nil
  "The scheduled idle timer for the next chunk, if any.")

(defvar night/h-olpl-pending-buffers nil
  "Buffers whose drain is queued but not yet armed.
Arming is deferred to a command-loop boundary: timers (idle or
wall-clock) fire during any `sit-for' — including ones inside the very
command that opened the file, since Lisp execution does not reset the
user-idle clock. Scheduling a timer immediately would therefore let the
drain hijack and stretch the opening command itself (observed: a 1s
`find-file' stretched to minutes). `post-command-hook' is the only safe
start signal: redisplay-time hooks like
`window-buffer-change-functions' are unsafe too, since redisplay also
runs inside the opening command's `sit-for's. The cost: for a buffer
opened by a background server eval, the drain starts only at the user's
next command.")

(defun night/h-olpl-request-arm (buf)
  "Arm BUF's drain at the next command-loop boundary."
  (cl-pushnew buf night/h-olpl-pending-buffers)
  (add-hook 'post-command-hook #'night/h-olpl-arm-pending))

(defun night/h-olpl-arm-pending ()
  "Start the drains of all pending buffers. Self-removing."
  (remove-hook 'post-command-hook #'night/h-olpl-arm-pending)
  (let ((bufs night/h-olpl-pending-buffers))
    (setq night/h-olpl-pending-buffers nil)
    (dolist (buf bufs)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (and (or night/h-olpl-queue night/h-olpl-dirty-beg)
                     (not night/h-olpl-timer))
            (night/h-olpl-schedule buf)))))))

(defun night/h-olpl-element-bound (pos side)
  "Extend POS to the boundary of the element containing it.
SIDE is `beg' or `end'. Region scans must be element-aligned, or the
narrowed parse can misread partial elements (e.g. the inside of a src
block as top-level org)."
  (save-excursion
    (goto-char pos)
    (let* ((el (org-element-at-point))
           (eb (and el (org-element-property :begin el)))
           (ee (and el (org-element-property :end el))))
      (cond
       ((null el) pos)
       ((eq side 'beg) (min pos (or eb pos)))
       ;; POS at (or before) the element's start is already a boundary;
       ;; extending would swallow the FOLLOWING element — for a headline
       ;; its :end spans the entire subtree.
       ((and eb (<= pos eb)) pos)
       (t (max pos (or ee pos)))))))

(defconst night/h-olpl-math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"
  "Candidate starts of LaTeX fragments/environments.
Copied from `org-format-latex' (org 9.7, org.el). It overmatches
\(closing $, math-looking text in verbatim contexts), so each match
must be confirmed with `org-element-context'.")

(defun night/h-olpl-fragments (&optional beg end)
  "Collect LaTeX fragments/environments as marker-pair conses.
With BEG and END, scan only that region; callers must pass
element-aligned bounds (see `night/h-olpl-element-bound').

Scans `night/h-olpl-math-regexp' and confirms each candidate with the
cache-backed `org-element-context' — the `org-format-latex' technique.
Cost is proportional to the number of math candidates, unlike
`org-element-parse-buffer', which ignores the element cache and
reparses the entire buffer."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max)))
        (frags nil))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward night/h-olpl-math-regexp end t)
        (let ((context (org-element-context)))
          (when (memq (org-element-type context)
                      '(latex-fragment latex-environment))
            (let ((fb (org-element-property :begin context))
                  (fe (org-element-property :end context)))
              (push (cons (copy-marker fb) (copy-marker fe)) frags)
              ;; Skip the fragment's remainder (e.g. its closing $).
              (goto-char fe))))))
    (nreverse frags)))

(defun night/h-olpl-candidate-count (beg end limit)
  "Count `night/h-olpl-math-regexp' matches in BEG..END, up to LIMIT.
Raw regexp matches only, no element parsing — the cheap dispatch bound
for `night/h-olpl-around-org--latex-preview-region'."
  (let ((count 0))
    (save-excursion
      (goto-char beg)
      (while (and (< count limit)
                  (re-search-forward night/h-olpl-math-regexp end t))
        (cl-incf count)))
    count))

(defun night/h-olpl-merge (frags)
  "Merge FRAGS into the queue, skipping duplicates. Returns the count added."
  (let ((seen (delq nil (mapcar (lambda (f) (marker-position (car f)))
                                night/h-olpl-queue)))
        (added 0))
    (dolist (frag frags)
      (cond
       ((memq (marker-position (car frag)) seen)
        (set-marker (car frag) nil)
        (set-marker (cdr frag) nil))
       (t
        (cl-incf added)
        (setq night/h-olpl-queue (nconc night/h-olpl-queue (list frag))))))
    added))

;;; Paste tracking: fragments PASTED after the initial scan would
;;; otherwise never render (the queue is a one-shot snapshot, and a
;;; finished drain disarms itself; org-fragtog only previews the
;;; fragment point exits, so bulk pastes slip through — typed LaTeX, by
;;; contrast, needs nothing beyond fragtog). The `insert-for-yank'
;;; advice below widens a dirty region on paste; the next tick rescans
;;; just that region and merges any new fragments into the queue.
(defvar-local night/h-olpl-dirty-beg nil
  "Marker at the start of the region pasted since the last scan, or nil.")

(defvar-local night/h-olpl-dirty-end nil
  "Marker at the end of the region pasted since the last scan, or nil.")

(defun night/h-olpl-dirty-clear ()
  (when night/h-olpl-dirty-beg (set-marker night/h-olpl-dirty-beg nil))
  (when night/h-olpl-dirty-end (set-marker night/h-olpl-dirty-end nil))
  (setq night/h-olpl-dirty-beg nil
        night/h-olpl-dirty-end nil))

(defun night/h-olpl-dirty-note (beg end)
  "Widen the dirty region to cover BEG..END and arm the drain."
  (cond
   ((null night/h-olpl-dirty-beg)
    (setq night/h-olpl-dirty-beg (copy-marker beg)
          night/h-olpl-dirty-end (copy-marker end t)))
   (t
    (when (< beg night/h-olpl-dirty-beg)
      (set-marker night/h-olpl-dirty-beg beg))
    (when (> end night/h-olpl-dirty-end)
      (set-marker night/h-olpl-dirty-end end))))
  (unless night/h-olpl-timer
    (night/h-olpl-request-arm (current-buffer))))

(defun night/h-olpl-absorb-dirty ()
  "Rescan the dirty region into the queue. No-op when nothing changed.
Already-previewed fragments are skipped, as is the fragment containing
point: org-fragtog previews that one when point exits it, and compiling
it mid-edit would render half-typed LaTeX."
  (when night/h-olpl-dirty-beg
    (let ((beg (night/h-olpl-element-bound
                (marker-position night/h-olpl-dirty-beg) 'beg))
          (end (night/h-olpl-element-bound
                (marker-position night/h-olpl-dirty-end) 'end)))
      (night/h-olpl-dirty-clear)
      (night/h-olpl-merge
       (cl-remove-if
        (lambda (frag)
          (let ((fb (marker-position (car frag)))
                (fe (marker-position (cdr frag))))
            (when (or (night/h-olpl-previewed-p fb)
                      (and (>= (point) fb) (< (point) fe)))
              (set-marker (car frag) nil)
              (set-marker (cdr frag) nil)
              t)))
        (night/h-olpl-fragments beg end))))))

(defun night/h-olpl-previewed-p (pos)
  "Whether POS already carries an org LaTeX preview overlay."
  (cl-some (lambda (ov)
             (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))
           (overlays-at pos)))

(defun night/h-olpl-preview-1 (frag)
  "Compile the preview for FRAG, a (BEGIN-MARKER . END-MARKER) cons.
Returns non-nil when a compile actually ran (as opposed to skipping a
dead or already-previewed fragment). Frees the markers afterwards;
errors are reported but do not abort the queue."
  (let ((beg (marker-position (car frag)))
        (end (marker-position (cdr frag))))
    (unwind-protect
        (cond
         ((not (and beg end)) nil)
         ((night/h-olpl-previewed-p beg) nil)
         (t
          (condition-case err
              ;; The reroute advice must not intercept the drain's own
              ;; compiles.
              (let ((night/h-olpl-inhibit-reroute t))
                (org--latex-preview-region beg end))
            (error
             (message "night/org-latex-preview-lazy: error at %d: %s"
                      beg (error-message-string err))))
          t))
      (set-marker (car frag) nil)
      (set-marker (cdr frag) nil))))

(defun night/h-olpl-priority (frag window-start window-end)
  "Sort key for FRAG: visible fragments first, top to bottom, then by
distance from the window."
  (let ((pos (or (marker-position (car frag)) most-positive-fixnum)))
    (cond
     ((and (>= pos window-start) (<= pos window-end)) pos)
     (t (+ window-end (abs (- pos window-start)))))))

(defun night/h-olpl-resort ()
  "Reorder the queue so fragments in the selected window come first."
  (let ((ws (window-start))
        (we (window-end nil t)))
    (setq night/h-olpl-queue
          (sort night/h-olpl-queue
                (lambda (a b)
                  (< (night/h-olpl-priority a ws we)
                     (night/h-olpl-priority b ws we)))))))

(defun night/h-olpl-schedule (buf &optional resting)
  "Schedule the next tick for BUF.
With RESTING non-nil, the drain is in progress and the next chunk runs
after a short wall-clock rest (so the event loop can serve input and
process output in between). Otherwise wait for the user to be idle for
`night/org-latex-preview-lazy-idle-delay'."
  (setq night/h-olpl-timer
        (cond
         (resting
          (run-with-timer night/org-latex-preview-lazy-rest-delay
                          nil #'night/h-olpl-tick buf))
         (t
          (run-with-idle-timer night/org-latex-preview-lazy-idle-delay
                               nil #'night/h-olpl-tick buf)))))

(defun night/h-olpl-tick (buf)
  "Compile one chunk of BUF's queue, then reschedule or finish.
The queue is re-prioritized towards BUF's current viewport first, so
previews always follow where the user is looking. When the user is
actively working (not idle, or input is pending), no compiling happens
and the drain parks on an idle timer instead."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq night/h-olpl-timer nil)
      (cond
       ;; Kill switch flipped while a tick was already scheduled.
       ((not night/org-latex-preview-lazy-enabled-p)
        (night/org-latex-preview-lazy-stop))
       ;; User became active (or is in the minibuffer): yield immediately,
       ;; resume on next idleness.
       ((or (input-pending-p)
            (not (current-idle-time))
            (active-minibuffer-window))
        (night/h-olpl-schedule buf))
       (t
        (night/h-olpl-absorb-dirty)
        (let ((win (get-buffer-window buf)))
          (when win
            (with-selected-window win
              (night/h-olpl-resort))))
        ;; Work until the tick's time budget is spent: cache hits and
        ;; skips cost ~1ms each and flow through in bulk, cold compiles
        ;; cap the tick at about one fragment.
        (let ((deadline (+ (float-time) night/org-latex-preview-lazy-tick-seconds))
              (worked nil))
          (while (and night/h-olpl-queue (< (float-time) deadline))
            (when (night/h-olpl-preview-1 (pop night/h-olpl-queue))
              (setq worked t)))
          (cond
           (night/h-olpl-queue (night/h-olpl-schedule buf 'resting))
           (t
            ;; Stay quiet on ticks that only absorbed fragment-free
            ;; pastes.
            (when worked
              (message "night/org-latex-preview-lazy: all previews done"))
            (night/org-latex-preview-lazy-stop)))))))))

(defun night/org-latex-preview-lazy-stop ()
  "Cancel lazy previewing in the current buffer, freeing all state.
The global paste advice re-arms the drain on the next paste; there is
no per-buffer state to keep beyond this."
  (interactive)
  (setq night/h-olpl-pending-buffers
        (delq (current-buffer) night/h-olpl-pending-buffers))
  (when night/h-olpl-timer
    (cancel-timer night/h-olpl-timer)
    (setq night/h-olpl-timer nil))
  (dolist (frag night/h-olpl-queue)
    (set-marker (car frag) nil)
    (set-marker (cdr frag) nil))
  (setq night/h-olpl-queue nil)
  (night/h-olpl-dirty-clear))

(defun night/h-olpl-usable-p ()
  "Whether the lazy machinery can run, signaling `user-error' when not."
  (cond
   ((not night/org-latex-preview-lazy-enabled-p)
    (user-error
     "night/org-latex-preview-lazy: disabled here; toggle with night/org-latex-preview-lazy-toggle (or -global-toggle)"))
   ((not (derived-mode-p 'org-mode))
    (user-error "night/org-latex-preview-lazy: not an org buffer"))
   ((night/org-latex-preview-new-system-p)
    (user-error
     "The new async org-latex-preview system is installed; use it directly"))
   ((not (fboundp 'org--latex-preview-region))
    (user-error
     "`org--latex-preview-region' is missing; org internals have changed"))
   (t t)))

(defun night/org-latex-preview-lazy ()
  "Preview all LaTeX fragments progressively without freezing Emacs.

Fragments visible in the window are compiled first, the rest from idle
timers within a per-tick time budget
\(`night/org-latex-preview-lazy-tick-seconds'). Scrolling
re-prioritizes the queue towards the viewport. Stop with
`night/org-latex-preview-lazy-stop'."
  (interactive)
  (when (night/h-olpl-usable-p)
    (night/org-latex-preview-lazy-stop)
    (setq night/h-olpl-queue (night/h-olpl-fragments))
    (cond
     ((not night/h-olpl-queue)
      (message "night/org-latex-preview-lazy: no LaTeX fragments found"))
     (t
      (message "night/org-latex-preview-lazy: previewing %d fragments ..."
               (length night/h-olpl-queue))
      ;; Never compile synchronously here, and never schedule a timer
      ;; directly: this function may be running inside `org-mode'
      ;; initialization (STARTUP latexpreview) or another command whose
      ;; `sit-for's would fire our timers and stretch that command. Arm at
      ;; the next command-loop boundary instead.
      (night/h-olpl-request-arm (current-buffer))))))

(defun night/org-latex-preview-lazy-region (beg end)
  "Queue the LaTeX fragments between BEG and END for lazy previewing.
Like `night/org-latex-preview-lazy' but region-bounded, and it MERGES
into any in-progress queue instead of restarting it. BEG and END are
extended to element boundaries."
  (interactive "r")
  (when (night/h-olpl-usable-p)
    (let* ((beg (night/h-olpl-element-bound beg 'beg))
           (end (night/h-olpl-element-bound end 'end))
           (added (night/h-olpl-merge (night/h-olpl-fragments beg end))))
      (cond
       ((and (zerop added) (not night/h-olpl-queue))
        (message
         "night/org-latex-preview-lazy-region: no LaTeX fragments found"))
       (t
        (when (> added 0)
          (message "night/org-latex-preview-lazy-region: queued %d fragment(s) ..."
                   added))
        (night/h-olpl-request-arm (current-buffer)))))))

;;;
(defun night/org-latex-preview-pin-toggle ()
  "Toggle whether LaTeX previews stay rendered when point enters them.

Stock org previews are sticky: they only disappear on text modification
or explicit toggling. The auto-hide-under-cursor behavior comes from
`org-fragtog-mode' (enabled per buffer by
`night/org-interactive-startup'). Pinning simply disables fragtog in
this buffer and restores the fragments it left raw; unpinning re-enables
fragtog. Note that *editing* a fragment's text still removes its
preview — that is org's own overlay behavior, independent of fragtog."
  (interactive)
  (cond
   ((bound-and-true-p org-fragtog-mode)
    (org-fragtog-mode -1)
    (night/org-latex-preview-lazy)
    (message "night/org-latex-preview-pin-toggle: pinned (fragtog off)"))
   (t
    (org-fragtog-mode 1)
    (message "night/org-latex-preview-pin-toggle: unpinned (fragtog on)"))))

(defvar night/org-latex-preview-pin-global-p nil
  "Whether LaTeX previews are pinned globally (org-fragtog suppressed).
Consulted by `night/org-interactive-startup' when opening new org
buffers; toggle with `night/org-latex-preview-pin-global-toggle'.")

(defun night/org-latex-preview-pin-global-toggle ()
  "Toggle pinned previews across all org buffers, current and future.

Simple stomp semantics: pinning disables `org-fragtog-mode' in every
existing org buffer (restoring raw fragments lazily) and suppresses it
for future buffers via `night/org-latex-preview-pin-global-p';
unpinning re-enables fragtog in all org buffers of graphical sessions
(mirroring `night/org-interactive-startup'). Per-buffer overrides made
with `night/org-latex-preview-pin-toggle' survive only until the next
global toggle."
  (interactive)
  (setq night/org-latex-preview-pin-global-p
        (not night/org-latex-preview-pin-global-p))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode)
        (cond
         (night/org-latex-preview-pin-global-p
          (when (bound-and-true-p org-fragtog-mode)
            (org-fragtog-mode -1)
            (night/org-latex-preview-lazy)))
         (t
          (when (display-graphic-p)
            (org-fragtog-mode 1)))))))
  (message "night/org-latex-preview-pin-global-toggle: %s"
           (cond
            (night/org-latex-preview-pin-global-p "pinned globally")
            (t "unpinned globally (fragtog on)"))))

;;;
(defun night/org-latex-preview-lazy-toggle ()
  "Toggle the lazy preview machinery in this buffer (emergency escape).
While disabled, the `org--latex-preview-region' and `insert-for-yank'
advices pass straight through, restoring stock synchronous previews.
Buffer-local; a later `night/org-latex-preview-lazy-global-toggle'
stomps it."
  (interactive)
  (setq-local night/org-latex-preview-lazy-enabled-p
              (not night/org-latex-preview-lazy-enabled-p))
  (unless night/org-latex-preview-lazy-enabled-p
    ;; Emergency semantics: halt any in-flight drain now.
    (night/org-latex-preview-lazy-stop))
  (message "night/org-latex-preview-lazy-toggle: %s in this buffer (globally: %s)"
           (if night/org-latex-preview-lazy-enabled-p "enabled" "disabled")
           (if (default-value 'night/org-latex-preview-lazy-enabled-p)
               "enabled" "disabled")))

(defun night/org-latex-preview-lazy-global-toggle ()
  "Toggle the lazy preview machinery everywhere (emergency escape).
Simple stomp semantics: flips the global default and discards all
buffer-local overrides made with `night/org-latex-preview-lazy-toggle'.
Disabling also halts every in-flight drain."
  (interactive)
  (setq-default night/org-latex-preview-lazy-enabled-p
                (not (default-value 'night/org-latex-preview-lazy-enabled-p)))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (kill-local-variable 'night/org-latex-preview-lazy-enabled-p)
      (unless (default-value 'night/org-latex-preview-lazy-enabled-p)
        (when (derived-mode-p 'org-mode)
          (night/org-latex-preview-lazy-stop)))))
  (unless (default-value 'night/org-latex-preview-lazy-enabled-p)
    (setq night/h-olpl-pending-buffers nil))
  (message "night/org-latex-preview-lazy-global-toggle: %s globally"
           (if (default-value 'night/org-latex-preview-lazy-enabled-p)
               "enabled" "disabled")))

;;;
(defun night/h-olpl-overlay-image-file (beg end)
  "Image file shown by the preview overlay covering BEG..END, if any."
  (cl-some (lambda (ov)
             (and (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
                  (plist-get (cdr (overlay-get ov 'display)) :file)))
           (overlays-in beg end)))

(defun night/h-olpl-cache-file (value &optional pos)
  "The cache image path `org-format-latex' would use for fragment text VALUE.
Mirrors the hash computation in org 9.7's `org-format-latex' as called
by `org--latex-preview-region' (FORBUFFER non-nil, prefix
\"org-ltximg\" under `org-preview-latex-image-directory'). POS is where
the fragment starts, used for `auto' color resolution. Note the
`:foreground'/`:background' entries of `org-format-latex-options'
resolve against the CURRENT theme/faces, so images rendered under a
different theme hash differently and will not be found."
  (let* ((processing-info
          (cdr (assq org-preview-latex-default-process
                     org-preview-latex-process-alist)))
         (imagetype (or (plist-get processing-info :image-output-type) "png"))
         (face (save-excursion
                 (when pos (goto-char pos))
                 (face-at-point)))
         (fg (let ((color (plist-get org-format-latex-options :foreground)))
               (cond
                ((eq color 'auto) (face-attribute face :foreground nil 'default))
                ((eq color 'default) (face-attribute 'default :foreground nil))
                (t color))))
         (bg (let ((color (plist-get org-format-latex-options :background)))
               (cond
                ((eq color 'auto) (face-attribute face :background nil 'default))
                ((eq color 'default) (face-attribute 'default :background nil))
                (t color))))
         (hash (sha1 (prin1-to-string
                      (list org-format-latex-header
                            org-latex-default-packages-alist
                            org-latex-packages-alist
                            org-format-latex-options
                            'forbuffer value fg bg))))
         (absprefix (expand-file-name
                     (concat org-preview-latex-image-directory "org-ltximg")
                     default-directory)))
    (format "%s_%s.%s" absprefix hash imagetype)))

(defun night/h-olpl-all-cached-p (frags)
  "Whether every fragment in FRAGS already has a cached preview image.
Short-circuits on the first miss, so cold buffers pay for roughly one
hash + one `file-exists-p' (~20µs). Callers must bound (length FRAGS)
BEFORE calling (see `night/org-latex-preview-lazy-sync-cached-max') —
that bounds the checking cost itself, not just the render."
  (cl-every
   (lambda (frag)
     (let ((beg (marker-position (car frag))))
       (and beg
            (let ((context (save-excursion
                             (goto-char beg)
                             (org-element-context))))
              (and (memq (org-element-type context)
                         '(latex-fragment latex-environment))
                   (file-exists-p
                    (night/h-olpl-cache-file
                     (org-element-property :value context) beg)))))))
   frags))

(defun night/org-latex-preview-cache-clear-buffer ()
  "Delete the cached preview images of this buffer's LaTeX fragments.

The cache dir (`org-preview-latex-image-directory') is SHARED across
all org files and keyed by content hash, so:
- a fragment with identical text in another file shares the same image
  file; clearing here cold-caches that file too;
- only images matching the current options and theme-resolved colors
  can be located (see `night/h-olpl-cache-file').
Previewed fragments' images are located exactly via their overlays;
un-previewed ones by recomputing the hash. Preview overlays in the
buffer are cleared as well, so the next preview command recompiles from
scratch."
  (interactive)
  (cond
   ((not (derived-mode-p 'org-mode))
    (user-error
     "night/org-latex-preview-cache-clear-buffer: not an org buffer"))
   (t
    ;; Stop any running drain first: with the cache gone it would start
    ;; recompiling everything it had left.
    (night/org-latex-preview-lazy-stop)
    (let ((deleted 0)
          (absent 0))
      (org-element-map (org-element-parse-buffer)
          '(latex-fragment latex-environment)
        (lambda (el)
          (let* ((beg (org-element-property :begin el))
                 (end (org-element-property :end el))
                 (file (or (night/h-olpl-overlay-image-file beg end)
                           (night/h-olpl-cache-file
                            (org-element-property :value el) beg))))
            (cond
             ((and file (file-exists-p file))
              (delete-file file)
              (cl-incf deleted))
             (t (cl-incf absent))))))
      (org-clear-latex-preview (point-min) (point-max))
      (message
       "night/org-latex-preview-cache-clear-buffer: deleted %d image(s) (%d had no cached image)"
       deleted absent)))))

;;;
;; Make lazy previewing the DEFAULT for every multi-fragment preview
;; path with a single choke-point advice: `org--latex-preview-region' is
;; what every synchronous preview funnels through — all
;; `org-latex-preview' branches (whole-buffer `C-u C-u' incl.
;; `#+STARTUP: latexpreview' during `org-mode' init, active region,
;; no-prefix section, the single-fragment toggle), Doom's
;; `+org/dwim-at-point'/`night/org-dwim-at-point' headline branch, and
;; `night/org-latex-preview-buffer'. Advising it covers every current
;; and future caller with no per-site patching. The clearing paths
;; (`C-u', `C-u C-u C-u') never reach it.
(defun night/h-olpl-around-org--latex-preview-region (orig-fn beg end)
  (cond
   ((or (not night/org-latex-preview-lazy-enabled-p) ;; kill switch
        night/h-olpl-inhibit-reroute ;; the drain's own compiles
        (night/org-latex-preview-new-system-p)
        (not (display-graphic-p)))
    (funcall orig-fn beg end))
   ;; Cheap dispatch: a raw regexp count with early exit — no element
   ;; parsing. The org-fragtog exit-re-render hot path lands here at the
   ;; cost of one bounded C-level regexp search. (A single $...$ yields
   ;; 2 candidates, both $s, so this fast path is conservative; the
   ;; precise scan below still syncs real counts <= the threshold.)
   ((<= (night/h-olpl-candidate-count
         beg end (1+ night/org-latex-preview-lazy-sync-threshold))
        night/org-latex-preview-lazy-sync-threshold)
    (funcall orig-fn beg end))
   (t
    (let* ((beg (night/h-olpl-element-bound beg 'beg))
           (end (night/h-olpl-element-bound end 'end))
           (frags (night/h-olpl-fragments beg end))
           (count (length frags)))
      (cond
       ((or (<= count night/org-latex-preview-lazy-sync-threshold)
            ;; All-cached regions render synchronously at ~0.25ms per
            ;; fragment ("cached => instant"). The count cap is checked
            ;; BEFORE any hashing so it bounds the check itself too.
            (and (<= count night/org-latex-preview-lazy-sync-cached-max)
                 (night/h-olpl-all-cached-p frags)))
        (dolist (frag frags)
          (set-marker (car frag) nil)
          (set-marker (cdr frag) nil))
        (funcall orig-fn beg end))
       (t
        (let ((added (night/h-olpl-merge frags)))
          (when (> added 0)
            (message "night/org-latex-preview-lazy: queued %d fragment(s) ..."
                     added))
          (night/h-olpl-request-arm (current-buffer)))))))))

;; Pasted fragments: `insert-for-yank' is the paste choke point —
;; `yank'/`yank-pop', evil's `p'/`P' (evil-commands.el inserts via it so
;; yank-handlers work), `org-yank', mouse yanks, and this config's
;; night/org-paste-* helpers (via `night/insert-for-yank') all funnel
;; through it. Typed LaTeX needs no handling: org-fragtog previews a
;; fragment when point exits it; bulk pastes are what slip through.
(defun night/h-olpl-around-insert-for-yank (orig-fn string &rest args)
  (let ((paste-beg (point)))
    (prog1 (apply orig-fn string args)
      (when (and night/org-latex-preview-lazy-enabled-p ;; kill switch
                 (derived-mode-p 'org-mode)
                 (display-graphic-p)
                 (not (night/org-latex-preview-new-system-p))
                 (fboundp 'org--latex-preview-region))
        (night/h-olpl-dirty-note paste-beg (point))))))

(after! org
  (advice-add #'org--latex-preview-region
              :around #'night/h-olpl-around-org--latex-preview-region)
  (advice-add #'insert-for-yank
              :around #'night/h-olpl-around-insert-for-yank))

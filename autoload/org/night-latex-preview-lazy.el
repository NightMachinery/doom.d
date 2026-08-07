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
  "Regions with at most this many UNCACHED fragments compile synchronously.
Used by `night/h-olpl-around-org--latex-preview-region'. The cost
driver is cold compiles (~0.3-1s each versus ~1ms for cache hits), so
this bounds the worst-case synchronous freeze to that many LaTeX runs
— which keeps the fragment-at-point toggle and org-fragtog's exit
re-renders instant, and lets a mostly-cached region (e.g. one edited
formula among hundreds of cached ones) still render immediately.")

(defvar night/org-latex-preview-lazy-sync-cached-max 5000
  "Cap on total fragments for the cache-checked synchronous path.
Regions up to this size whose uncached fragments do not exceed
`night/org-latex-preview-lazy-sync-threshold' render synchronously
\(warm renders cost ~0.25ms per fragment). The cap bounds both that
bulk render and the hash-checking itself (~10-30µs per fragment) on
pathological files; it is checked against the fragment count BEFORE
any hashes are computed. Keep it well above the sync threshold.")

(defvar night/h-olpl-inhibit-reroute nil
  "Bound non-nil by the lazy drain so its own
`org--latex-preview-region' calls reach the real function instead of
being rerouted back into the queue.")

(defvar night/org-latex-preview-lazy-mode 'bg
  "How the lazy preview machinery operates. One of:

- `original': the `org--latex-preview-region' and `insert-for-yank'
  advices pass straight through to stock synchronous behavior, and
  the lazy commands refuse — the emergency kill switch.
- `timer-ticks': foreground drain only — cold fragments compile in
  the GUI, at most ~1 per tick.
- `timer+bg': the drain compiles <=1 cold fragment per tick WHILE
  background pipelines warm the rest of the cache in parallel.
- `bg' (default): fully event-driven, no queue and no timers —
  dispatch renders already-cached fragments synchronously on the spot
  (warm files render during the open itself) and pipelines the cold
  ones; each pipeline's sentinel renders its chunk the moment it
  lands. The foreground never runs LaTeX (see
  `night/org-latex-preview-lazy-bg-sync-threshold').

Set buffer-locally or globally with
`night/org-latex-preview-lazy-mode-set'; flip to/from `original' with
`night/org-latex-preview-lazy-toggle' /
`night/org-latex-preview-lazy-global-toggle'.")

(defvar-local night/h-olpl-saved-mode nil
  "The buffer's mode before `night/org-latex-preview-lazy-toggle'
switched it to `original', for switching back.")

(defvar night/h-olpl-saved-global-mode nil
  "The global mode before `night/org-latex-preview-lazy-global-toggle'
switched it to `original', for switching back.")

(defvar night/org-latex-preview-lazy-warm-workers nil
  "Max concurrent background compile pipelines. nil = `num-processors'.
No cap and no core reservation: contention with interactive work is
handled by priority instead — every pipeline command runs under
`nice -n 10', so the scheduler yields cores to the user on demand.")

(defvar night/org-latex-preview-lazy-warm-batch-size 20
  "Fragments per batched .tex chunk in the background pipelines.
Each chunk costs ONE preamble parse (the dominant per-fragment cost,
~250-400ms) amortized over the whole chunk, plus one multi-page
dvisvgm run. Moderate size bounds the blast radius of a broken
fragment (failed chunks retry per-fragment) and yields early partial
results.")

(defvar night/org-latex-preview-lazy-warm-min 2
  "TIMER+BG ONLY: minimum uncached backlog before pipelines spawn.
Below this the drain foreground-compiles as in `timer-ticks'. Default
2 aligns with the sync-threshold philosophy: at most ONE cold compile
may block the foreground. Mode `bg' ignores this — it dispatches every
cold count (a solo pipeline costs only ~30-50ms of fork overhead over
the identical foreground compile, with zero blocking).")

(defvar night/org-latex-preview-lazy-bg-sync-threshold 0
  "BG ONLY: regions with at most this many uncached fragments compile
synchronously (the old blocking behavior). The default 0 makes bg
fully async — the foreground never runs LaTeX, only places images; a
cold fragment's preview appears ~0.7s later instead of Emacs freezing
~0.7s. Set to 1 to restore the blocking-but-atomic feel for
single-fragment toggles (same wall latency either way).
`night/org-latex-preview-lazy-sync-threshold' governs the timer modes
only.")

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
          (cond
           ;; bg: event-driven — dispatch the pasted region directly
           ;; (we are at a command boundary; bg dispatch never blocks
           ;; on LaTeX). The queue is only in play via the rare
           ;; cached-overflow path, which still uses timers.
           ((eq night/org-latex-preview-lazy-mode 'bg)
            (when night/h-olpl-dirty-beg
              (let ((db (marker-position night/h-olpl-dirty-beg))
                    (de (marker-position night/h-olpl-dirty-end)))
                (night/h-olpl-dirty-clear)
                (when (and db de)
                  (night/h-olpl-bg-dispatch db de))))
            (when (and night/h-olpl-queue (not night/h-olpl-timer))
              (night/h-olpl-schedule buf)))
           (t
            (when (and (or night/h-olpl-queue night/h-olpl-dirty-beg)
                       (not night/h-olpl-timer))
              (night/h-olpl-schedule buf)))))))))

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
    (when (> added 0)
      ;; New fragments may need warming even if a previous warm run
      ;; already finished.
      (setq night/h-olpl-warm-done nil))
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

(defun night/h-olpl-render-cached (beg end tofile)
  "Place the preview overlay for the fragment at BEG..END from TOFILE.
The direct equivalent of `org-format-latex''s cache-hit path
\(trimmed overlay end + `org--make-preview-overlay'), WITHOUT its
whole-buffer `clear-image-cache' flush and regexp scan. The flush
matters enormously: org clears Emacs's entire rasterized-image cache
on EVERY call, forcing every visible preview to re-decode at the next
redisplay — with hundreds of per-fragment renders this starved the
main loop (observed: 248s for a warming run whose subprocesses took
~5s). Our files are content-addressed (fresh hash filenames), so
there is never a stale cache entry to flush. (Do NOT stub the
primitive via cl-letf instead: redefining a C subr makes native-comp
build a trampoline, which ICEs when the gcc driver is unavailable.)"
  (let ((imagetype (or (plist-get
                        (cdr (assq org-preview-latex-default-process
                                   org-preview-latex-process-alist))
                        :image-output-type)
                       "png"))
        (ov-end (save-excursion
                  (goto-char end)
                  (skip-chars-backward " \r\t\n")
                  (point))))
    (org--make-preview-overlay beg ov-end tofile imagetype)))

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
              (let* ((context (save-excursion (goto-char beg)
                                              (org-element-context)))
                     (tofile (and (memq (org-element-type context)
                                        '(latex-fragment latex-environment))
                                  (night/h-olpl-cache-file
                                   (org-element-property :value context)
                                   beg))))
                (cond
                 ;; Cache hit: place the overlay directly — avoids
                 ;; org-format-latex's per-call whole-cache image flush
                 ;; (see `night/h-olpl-render-cached').
                 ((and tofile (file-exists-p tofile))
                  (night/h-olpl-render-cached beg end tofile))
                 (t
                  ;; Cold (or content changed): stock synchronous
                  ;; compile; org's one flush per compile is stock
                  ;; cadence.
                  (let ((night/h-olpl-inhibit-reroute t))
                    (org--latex-preview-region beg end)))))
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
       ((eq night/org-latex-preview-lazy-mode 'original)
        (night/org-latex-preview-lazy-stop))
       ;; User became active (or is in the minibuffer): yield immediately,
       ;; resume on next idleness.
       ((or (input-pending-p)
            (not (current-idle-time))
            (active-minibuffer-window))
        (night/h-olpl-schedule buf))
       (t
        (night/h-olpl-absorb-dirty)
        (when (memq night/org-latex-preview-lazy-mode '(timer+bg bg))
          (night/h-olpl-warm-maybe-start))
        (let ((win (get-buffer-window buf)))
          (when win
            (with-selected-window win
              (night/h-olpl-resort))))
        ;; Work until the tick's time budget is spent: cache hits and
        ;; skips cost ~1ms each and flow through in bulk, cold compiles
        ;; cap the tick at about one fragment. In the bg modes,
        ;; fragments being warmed (or, in `bg', any cold fragment) are
        ;; deferred to the queue tail instead of compiled here; ticks
        ;; keep cycling at the rest cadence, sweeping cache hits as the
        ;; pipelines produce them.
        (let* ((mode night/org-latex-preview-lazy-mode)
               (bg-defer-p (and (eq mode 'bg)
                                (not night/h-olpl-warm-smallp)))
               (check-warm-p (memq mode '(timer+bg bg)))
               (deadline (+ (float-time) night/org-latex-preview-lazy-tick-seconds))
               (worked nil)
               (deferred nil)
               (dropped 0))
          (while (and night/h-olpl-queue (< (float-time) deadline))
            (let* ((frag (pop night/h-olpl-queue))
                   (status (if check-warm-p
                               (night/h-olpl-frag-warm-status frag)
                             'ready)))
              (cond
               ((eq status 'ready)
                (when (night/h-olpl-preview-1 frag)
                  (setq worked t)))
               ((eq status 'inflight)
                (push frag deferred))
               ;; cold:
               ((and bg-defer-p (not night/h-olpl-warm-done))
                (push frag deferred))
               (bg-defer-p
                ;; Warming finished without producing this one: it
                ;; failed to compile. Drop it rather than loop forever.
                (set-marker (car frag) nil)
                (set-marker (cdr frag) nil)
                (cl-incf dropped))
               (t ;; timer+bg (or bg below warm-min): foreground compile
                (when (night/h-olpl-preview-1 frag)
                  (setq worked t))))))
          (when deferred
            (setq night/h-olpl-queue
                  (nconc night/h-olpl-queue (nreverse deferred))))
          (when (> dropped 0)
            (message "night/org-latex-preview-lazy: dropped %d fragment(s) that failed to compile"
                     dropped))
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
  (night/h-olpl-dirty-clear)
  (night/h-olpl-warm-cancel))

(defun night/h-olpl-usable-p ()
  "Whether the lazy machinery can run, signaling `user-error' when not."
  (cond
   ((eq night/org-latex-preview-lazy-mode 'original)
    (user-error
     "night/org-latex-preview-lazy: mode is `original' here; change with night/org-latex-preview-lazy-mode-set (or the toggles)"))
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
    (cond
     ((eq night/org-latex-preview-lazy-mode 'bg)
      (night/h-olpl-bg-dispatch (point-min) (point-max)))
     (t
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
        (night/h-olpl-request-arm (current-buffer))))))))

(defun night/org-latex-preview-lazy-region (beg end)
  "Queue the LaTeX fragments between BEG and END for lazy previewing.
Like `night/org-latex-preview-lazy' but region-bounded, and it MERGES
into any in-progress queue instead of restarting it. BEG and END are
extended to element boundaries."
  (interactive "r")
  (when (night/h-olpl-usable-p)
    (cond
     ((eq night/org-latex-preview-lazy-mode 'bg)
      (night/h-olpl-bg-dispatch beg end))
     (t
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
          (night/h-olpl-request-arm (current-buffer)))))))))

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
(defconst night/h-olpl-modes '(original timer-ticks timer+bg bg)
  "Valid values of `night/org-latex-preview-lazy-mode'.")

(defun night/org-latex-preview-lazy-mode-set (mode &optional globalp)
  "Set the lazy preview MODE (see `night/org-latex-preview-lazy-mode').
Sets it buffer-locally; with GLOBALP (interactively, a prefix arg) sets
the global default and stomps all buffer-local overrides."
  (interactive
   (list (intern (completing-read
                  "Lazy preview mode: "
                  night/h-olpl-modes nil t))
         current-prefix-arg))
  (unless (memq mode night/h-olpl-modes)
    (user-error "Unknown lazy preview mode: %s" mode))
  (cond
   (globalp
    (setq-default night/org-latex-preview-lazy-mode mode)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (kill-local-variable 'night/org-latex-preview-lazy-mode)
        (kill-local-variable 'night/h-olpl-saved-mode)
        (when (derived-mode-p 'org-mode)
          (cond
           ((eq mode 'original) (night/org-latex-preview-lazy-stop))
           ((not (memq mode '(timer+bg bg))) (night/h-olpl-warm-cancel))))))
    (when (eq mode 'original)
      (setq night/h-olpl-pending-buffers nil)))
   (t
    (setq-local night/org-latex-preview-lazy-mode mode)
    (cond
     ((eq mode 'original) (night/org-latex-preview-lazy-stop))
     ((not (memq mode '(timer+bg bg))) (night/h-olpl-warm-cancel)))))
  (message "night/org-latex-preview-lazy-mode-set: %s %s" mode
           (if globalp "globally (buffer-locals stomped)" "in this buffer")))

(defun night/org-latex-preview-lazy-toggle ()
  "Toggle this buffer between mode `original' and its last active mode.
The emergency kill switch: in `original' the advices pass straight
through to stock synchronous previews. Buffer-local; a later
`night/org-latex-preview-lazy-global-toggle' stomps it."
  (interactive)
  (cond
   ((eq night/org-latex-preview-lazy-mode 'original)
    (setq-local night/org-latex-preview-lazy-mode
                (or night/h-olpl-saved-mode
                    (let ((def (default-value 'night/org-latex-preview-lazy-mode)))
                      (if (eq def 'original) 'bg def)))))
   (t
    (setq-local night/h-olpl-saved-mode night/org-latex-preview-lazy-mode)
    (setq-local night/org-latex-preview-lazy-mode 'original)
    ;; Emergency semantics: halt any in-flight drain and warming now.
    (night/org-latex-preview-lazy-stop)))
  (message "night/org-latex-preview-lazy-toggle: %s in this buffer (global default: %s)"
           night/org-latex-preview-lazy-mode
           (default-value 'night/org-latex-preview-lazy-mode)))

(defun night/org-latex-preview-lazy-global-toggle ()
  "Toggle everywhere between mode `original' and the last active mode.
Simple stomp semantics: flips the global default and discards all
buffer-local overrides. Switching to `original' halts every in-flight
drain and warming pipeline."
  (interactive)
  (cond
   ((eq (default-value 'night/org-latex-preview-lazy-mode) 'original)
    (setq-default night/org-latex-preview-lazy-mode
                  (or night/h-olpl-saved-global-mode 'bg)))
   (t
    (setq night/h-olpl-saved-global-mode
          (default-value 'night/org-latex-preview-lazy-mode))
    (setq-default night/org-latex-preview-lazy-mode 'original)))
  (let ((original-p (eq (default-value 'night/org-latex-preview-lazy-mode)
                        'original)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (kill-local-variable 'night/org-latex-preview-lazy-mode)
        (kill-local-variable 'night/h-olpl-saved-mode)
        (when (and original-p (derived-mode-p 'org-mode))
          (night/org-latex-preview-lazy-stop))))
    (when original-p
      (setq night/h-olpl-pending-buffers nil)))
  (message "night/org-latex-preview-lazy-global-toggle: %s globally"
           (default-value 'night/org-latex-preview-lazy-mode)))

;;;
(defun night/h-olpl-overlay-image-file (beg end)
  "Image file shown by the preview overlay covering BEG..END, if any."
  (cl-some (lambda (ov)
             (and (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay)
                  (plist-get (cdr (overlay-get ov 'display)) :file)))
           (overlays-in beg end)))

(defun night/h-olpl-resolved-colors (&optional pos)
  "Resolved (FG . BG) as org 9.7's `org-format-latex' computes them.
POS is the fragment start, used for `auto' color resolution. The
resolution consults the CURRENT theme/faces."
  (let ((face (save-excursion
                (when pos (goto-char pos))
                (face-at-point))))
    (cons
     (let ((color (plist-get org-format-latex-options :foreground)))
       (cond
        ((eq color 'auto) (face-attribute face :foreground nil 'default))
        ((eq color 'default) (face-attribute 'default :foreground nil))
        (t color)))
     (let ((color (plist-get org-format-latex-options :background)))
       (cond
        ((eq color 'auto) (face-attribute face :background nil 'default))
        ((eq color 'default) (face-attribute 'default :background nil))
        (t color))))))

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
         (colors (night/h-olpl-resolved-colors pos))
         (fg (car colors))
         (bg (cdr colors))
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

(defun night/h-olpl-uncached-count (frags limit)
  "Count fragments in FRAGS lacking a cached preview image, up to LIMIT.
Early-exits once the count reaches LIMIT, so a fully cold buffer pays
for roughly LIMIT hashes + `file-exists-p's (~20µs each). Dead markers
and non-fragment contexts count as uncached (conservative: they push
the caller toward the lazy path). Callers must bound (length FRAGS)
BEFORE calling (see `night/org-latex-preview-lazy-sync-cached-max') —
that bounds the checking cost itself, not just the render."
  (let ((count 0))
    (while (and frags (< count limit))
      (let* ((beg (marker-position (car (pop frags))))
             (cached
              (and beg
                   (let ((context (save-excursion
                                    (goto-char beg)
                                    (org-element-context))))
                     (and (memq (org-element-type context)
                                '(latex-fragment latex-environment))
                          (file-exists-p
                           (night/h-olpl-cache-file
                            (org-element-property :value context) beg)))))))
        (unless cached
          (cl-incf count))))
    count))

;;; Background cache warming: compile cold fragments OUTSIDE Emacs, in
;;; parallel, directly into the shared preview cache at the exact paths
;;; `night/h-olpl-cache-file' computes; the drain then renders them as
;;; ~1ms cache hits. No batch-Emacs workers: the GUI assembles batched
;;; multi-page .tex documents itself (mirroring org 9.7's
;;; `org-create-formula-image' assembly — ONE preamble parse amortized
;;; over `night/org-latex-preview-lazy-warm-batch-size' fragments) and
;;; runs `latex' + `dvisvgm' as sentinel-chained subprocesses under
;;; `nice'. Retired along with the rest of this file at the org 9.8
;;; async preview overhaul, which does all this and more natively.
(defvar-local night/h-olpl-warm-procs nil
  "Live pipeline processes of this buffer.")

(defvar-local night/h-olpl-warm-pending nil
  "Chunks waiting for a pipeline slot: list of (SOLO-RETRY-P . TASKS).
Each task is a plist (:value STRING :tofile PATH), plus, in `bg' mode,
:markers — the (BEGIN-MARKER . END-MARKER) conses of every fragment
sharing that content hash, rendered directly by the dvisvgm sentinel.")

(defvar-local night/h-olpl-warm-task-index nil
  "Hash: cache path -> its pending/in-flight task in THIS buffer.
Lets later `bg' dispatches attach additional fragments (duplicate
content, overlapping regions) to a compile that is already underway,
and lets the drain (timer+bg) skip in-flight hashes. Deliberately
buffer-local with no global registry: a stale global entry once
silently blocked 8 fragments from ever warming; the worst case now is
two buffers compiling the same hash concurrently, which is harmless
\(identical bytes, atomic rename).")

(defun night/h-olpl-warm-task-for (tofile)
  "This buffer's pending/in-flight task producing TOFILE, if any."
  (and night/h-olpl-warm-task-index
       (gethash tofile night/h-olpl-warm-task-index)))

(defun night/h-olpl-warm-register-task (task)
  "Register TASK in this buffer's task index."
  (unless night/h-olpl-warm-task-index
    (setq night/h-olpl-warm-task-index (make-hash-table :test 'equal)))
  (puthash (plist-get task :tofile) task night/h-olpl-warm-task-index))

(defvar-local night/h-olpl-warm-total-frags 0)
(defvar-local night/h-olpl-warm-done-frags 0)
(defvar-local night/h-olpl-warm-total-chunks 0)
(defvar-local night/h-olpl-warm-done-chunks 0)
(defvar-local night/h-olpl-warm-failed 0)

(defvar-local night/h-olpl-warm-render-info nil
  "Per-warm-run render inputs: plist (:header :fg :bg :scale).
Computed once when warming starts so every chunk (and per-fragment
retry) compiles identically.")

(defvar-local night/h-olpl-warm-smallp nil
  "Non-nil when the uncached backlog was below the warm minimum;
mode `bg' then foreground-compiles like `timer-ticks'.")

(defvar-local night/h-olpl-warm-done nil
  "Non-nil after a warm run finished; cold fragments still in the
queue then failed to compile and are dropped instead of deferred.
Reset when new fragments are merged.")

(defun night/h-olpl-warm-free-task (task)
  "Unregister TASK and free its fragment markers."
  (when night/h-olpl-warm-task-index
    (remhash (plist-get task :tofile) night/h-olpl-warm-task-index))
  (dolist (frag (plist-get task :markers))
    (set-marker (car frag) nil)
    (set-marker (cdr frag) nil)))

(defun night/h-olpl-warm-cancel ()
  "Kill this buffer's warm pipelines and clear all warming state."
  (dolist (proc night/h-olpl-warm-procs)
    (set-process-sentinel proc #'ignore)
    (ignore-errors (delete-process proc))
    (dolist (task (process-get proc 'olpl-chunk))
      (night/h-olpl-warm-free-task task))
    (let ((tmpdir (process-get proc 'olpl-tmpdir)))
      (when tmpdir (ignore-errors (delete-directory tmpdir t)))))
  (dolist (entry night/h-olpl-warm-pending)
    (dolist (task (cdr entry))
      (night/h-olpl-warm-free-task task)))
  (setq night/h-olpl-warm-procs nil
        night/h-olpl-warm-pending nil
        night/h-olpl-warm-render-info nil
        night/h-olpl-warm-task-index nil
        night/h-olpl-warm-smallp nil
        night/h-olpl-warm-done nil
        night/h-olpl-warm-total-frags 0
        night/h-olpl-warm-done-frags 0
        night/h-olpl-warm-total-chunks 0
        night/h-olpl-warm-done-chunks 0
        night/h-olpl-warm-failed 0))

(defun night/h-olpl-frag-warm-status (frag)
  "How the drain should treat FRAG: `ready', `inflight', or `cold'.
`ready' = hand to `night/h-olpl-preview-1' now (dead / previewed /
no-longer-a-fragment cases are cheap there; cached fragments render in
~1ms). `inflight' = a pipeline is producing its image. `cold' = needs
a LaTeX run."
  (let ((beg (marker-position (car frag))))
    (if (or (not beg) (night/h-olpl-previewed-p beg))
        'ready
      (let ((context (save-excursion (goto-char beg) (org-element-context))))
        (if (not (memq (org-element-type context)
                       '(latex-fragment latex-environment)))
            'ready
          (let ((tofile (night/h-olpl-cache-file
                         (org-element-property :value context) beg)))
            (cond
             ((file-exists-p tofile) 'ready)
             ((night/h-olpl-warm-task-for tofile) 'inflight)
             (t 'cold))))))))

(defun night/h-olpl-warm-collect-tasks ()
  "Tasks for queued, un-previewed, un-cached, not-in-flight fragments."
  (let ((tasks nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (frag night/h-olpl-queue)
      (let ((beg (marker-position (car frag))))
        (when (and beg (not (night/h-olpl-previewed-p beg)))
          (let ((context (save-excursion (goto-char beg)
                                         (org-element-context))))
            (when (memq (org-element-type context)
                        '(latex-fragment latex-environment))
              (let* ((value (org-element-property :value context))
                     (tofile (night/h-olpl-cache-file value beg)))
                (unless (or (file-exists-p tofile)
                            (gethash tofile seen)
                            (night/h-olpl-warm-task-for tofile))
                  (puthash tofile t seen)
                  (push (list :value value :tofile tofile) tasks))))))))
    (nreverse tasks)))

(defun night/h-olpl-warm-render-info ()
  "Compute the per-run render inputs, mirroring `org-create-formula-image'.
Header via `org-latex-make-preamble' (reads this buffer's
#+LATEX_HEADER keywords — the reason warming must be orchestrated from
the GUI, not a buffer-less batch process); fg/bg as LaTeX rgb triples;
scale as dvisvgm's --scale value."
  (require 'ox-latex nil t)
  (let* ((header
          (or (and (fboundp 'org-latex-make-preamble)
                   (ignore-errors
                     (org-latex-make-preamble
                      (org-export-get-environment (org-export-get-backend 'latex))
                      org-format-latex-header
                      'snippet)))
              org-format-latex-header))
         (colors (night/h-olpl-resolved-colors))
         (fg (let ((c (car colors)))
               (if (eq c 'default)
                   (org-latex-color :foreground)
                 (org-latex-color-format c))))
         (bg (let ((c (cdr colors)))
               (cond
                ((eq c 'default) (org-latex-color :background))
                ((equal c "Transparent") nil)
                (t (org-latex-color-format c)))))
         (processing-info
          (cdr (assq org-preview-latex-default-process
                     org-preview-latex-process-alist)))
         (image-size-adjust (or (plist-get processing-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (scale (* (car image-size-adjust)
                   (or (plist-get org-format-latex-options :scale) 1.0)))
         (dpi (* scale (if (and (display-graphic-p)
                                (fboundp 'org--get-display-dpi))
                           (org--get-display-dpi)
                         140.0))))
    (list :header header :fg fg :bg bg :scale (/ dpi 140.0))))

(defun night/h-olpl-warm-body (string)
  "Fragment body as `org-create-formula-image' would emit it.
Note: org 9.7 intends to replace a trailing newline with %, but its
`string-suffix-p' arguments are swapped, so in practice it always
appends % — mirror the ACTUAL behavior for identical output."
  (concat string "%"))

(defun night/h-olpl-warm-maybe-start ()
  "Start background warming when there is a worthwhile cold backlog.
No-op while a run is active or after one completed (until new
fragments arrive)."
  (when (and (not night/h-olpl-warm-procs)
             (not night/h-olpl-warm-pending)
             (not night/h-olpl-warm-done)
             night/h-olpl-queue)
    (let ((tasks (night/h-olpl-warm-collect-tasks)))
      (cond
       ((null tasks) nil)
       ((< (length tasks) night/org-latex-preview-lazy-warm-min)
        (setq night/h-olpl-warm-smallp t))
       (t
        (setq night/h-olpl-warm-smallp nil)
        (night/h-olpl-warm-launch tasks))))))

(defun night/h-olpl-bg-dispatch (beg end)
  "Event-driven `bg'-mode preview of BEG..END.
Renders already-cached fragments synchronously right now (warm files
render during the open itself) and pipelines the cold ones — their
sentinels render each chunk the moment it lands. No queue, no timers,
no idle waits, and (with `night/org-latex-preview-lazy-bg-sync-threshold'
at 0) no LaTeX ever runs in the foreground."
  (let* ((beg (night/h-olpl-element-bound beg 'beg))
         (end (night/h-olpl-element-bound end 'end))
         (frags (night/h-olpl-fragments beg end))
         (cached nil)
         (new-tasks nil))
    (unless night/h-olpl-warm-task-index
      (setq night/h-olpl-warm-task-index (make-hash-table :test 'equal)))
    ;; Partition.
    (dolist (frag frags)
      (let ((fb (marker-position (car frag))))
        (cond
         ((or (not fb) (night/h-olpl-previewed-p fb))
          (set-marker (car frag) nil)
          (set-marker (cdr frag) nil))
         (t
          (let ((context (save-excursion (goto-char fb)
                                         (org-element-context))))
            (cond
             ((not (memq (org-element-type context)
                         '(latex-fragment latex-environment)))
              (set-marker (car frag) nil)
              (set-marker (cdr frag) nil))
             (t
              (let* ((tofile (night/h-olpl-cache-file
                              (org-element-property :value context) fb))
                     (task (night/h-olpl-warm-task-for tofile)))
                (cond
                 ((file-exists-p tofile)
                  (push (cons frag tofile) cached))
                 (task
                  ;; This hash is already pending/in flight in this
                  ;; buffer: share the compile.
                  (plist-put task :markers
                             (cons frag (plist-get task :markers))))
                 (t
                  (let ((new (list :value (org-element-property :value context)
                                   :tofile tofile
                                   :markers (list frag))))
                    (night/h-olpl-warm-register-task new)
                    (push new new-tasks))))))))))))
    ;; Render the cached subset right now, bounded by the cap; the
    ;; pathological overflow (>cap warm fragments) goes through the
    ;; timer-queue machinery instead.
    (let ((n 0)
          (overflow nil))
      (dolist (entry (nreverse cached))
        (let* ((frag (car entry))
               (tofile (cdr entry))
               (fb (marker-position (car frag)))
               (fe (marker-position (cdr frag))))
          (cond
           ((not (and fb fe))
            (set-marker (car frag) nil)
            (set-marker (cdr frag) nil))
           ((>= n night/org-latex-preview-lazy-sync-cached-max)
            (push frag overflow))
           (t
            (cl-incf n)
            ;; Leave the fragment under point to org-fragtog.
            (unless (and (>= (point) fb) (< (point) fe))
              (night/h-olpl-render-cached fb fe tofile))
            (set-marker (car frag) nil)
            (set-marker (cdr frag) nil)))))
      (when overflow
        (night/h-olpl-merge (nreverse overflow))
        (night/h-olpl-request-arm (current-buffer))))
    ;; Dispatch the cold subset.
    (setq new-tasks (nreverse new-tasks))
    (cond
     ((null new-tasks) nil)
     ;; Opt-in blocking path for tiny cold counts.
     ((<= (length new-tasks)
          night/org-latex-preview-lazy-bg-sync-threshold)
      (dolist (task new-tasks)
        (night/h-olpl-warm-free-task task))
      (let ((night/h-olpl-inhibit-reroute t))
        (org--latex-preview-region beg end)))
     (t
      ;; Viewport-first COMPILE order: visible fragments' chunks land
      ;; (and render) first.
      (let ((win (get-buffer-window (current-buffer))))
        (when win
          (let ((ws (window-start win))
                (we (window-end win t)))
            (setq new-tasks
                  (sort new-tasks
                        (lambda (a b)
                          (< (night/h-olpl-priority
                              (car (plist-get a :markers)) ws we)
                             (night/h-olpl-priority
                              (car (plist-get b :markers)) ws we))))))))
      (night/h-olpl-warm-add new-tasks)))))

(defun night/h-olpl-warm-add (tasks)
  "Add TASKS to the warming run, launching one if none is active."
  (cond
   ((not (or night/h-olpl-warm-procs night/h-olpl-warm-pending))
    (night/h-olpl-warm-launch tasks))
   (t
    (dolist (task tasks)
      (night/h-olpl-warm-register-task task))
    (let* ((bs night/org-latex-preview-lazy-warm-batch-size)
           (n (length tasks))
           (chunks nil))
      (while tasks
        (push (cons nil (seq-take tasks bs)) chunks)
        (setq tasks (nthcdr bs tasks)))
      (setq chunks (nreverse chunks))
      (setq night/h-olpl-warm-pending
            (nconc night/h-olpl-warm-pending chunks))
      (cl-incf night/h-olpl-warm-total-frags n)
      (cl-incf night/h-olpl-warm-total-chunks (length chunks))
      (message "night/org-latex-preview-lazy: added %d fragment(s) to background warming"
               n))
    (let ((workers (or night/org-latex-preview-lazy-warm-workers
                       (num-processors))))
      (while (and night/h-olpl-warm-pending
                  (< (length night/h-olpl-warm-procs) workers))
        (night/h-olpl-warm-dispatch-next))))))

(defun night/h-olpl-warm-launch (tasks)
  "Chunk TASKS, initialize progress counters, and spawn the pipelines."
  (setq night/h-olpl-warm-done nil)
  (setq night/h-olpl-warm-render-info (night/h-olpl-warm-render-info))
  (add-hook 'kill-buffer-hook #'night/h-olpl-warm-cancel nil t)
  (dolist (task tasks)
    (night/h-olpl-warm-register-task task))
  (let ((bs night/org-latex-preview-lazy-warm-batch-size)
        (n (length tasks))
        (chunks nil))
    (while tasks
      (push (cons nil (seq-take tasks bs)) chunks)
      (setq tasks (nthcdr bs tasks)))
    (setq night/h-olpl-warm-pending (nreverse chunks)
          night/h-olpl-warm-total-frags n
          night/h-olpl-warm-done-frags 0
          night/h-olpl-warm-total-chunks (length night/h-olpl-warm-pending)
          night/h-olpl-warm-done-chunks 0
          night/h-olpl-warm-failed 0)
    (message "night/org-latex-preview-lazy: warming %d fragment(s) in %d chunk(s) in the background ..."
             n night/h-olpl-warm-total-chunks))
  (let ((workers (or night/org-latex-preview-lazy-warm-workers
                     (num-processors))))
    (dotimes (_ (min workers (length night/h-olpl-warm-pending)))
      (night/h-olpl-warm-dispatch-next))))

(defun night/h-olpl-warm-dispatch-next ()
  "Start the next pending chunk, or finish the run when none remain."
  (let ((entry (pop night/h-olpl-warm-pending)))
    (cond
     (entry
      (night/h-olpl-warm-compile-chunk (cdr entry) (car entry)))
     ((null night/h-olpl-warm-procs)
      (setq night/h-olpl-warm-done t
            night/h-olpl-warm-render-info nil
            night/h-olpl-warm-task-index nil)
      (message "night/org-latex-preview-lazy: background warming done (%d warmed%s)"
               night/h-olpl-warm-done-frags
               (if (> night/h-olpl-warm-failed 0)
                   (format ", %d FAILED" night/h-olpl-warm-failed)
                 ""))
      (night/h-olpl-request-arm (current-buffer))))))

(defun night/h-olpl-warm-compile-chunk (chunk solo-retry-p)
  "Compile CHUNK's fragments into one multi-page document, async."
  (let* ((info night/h-olpl-warm-render-info)
         (tmpdir (make-temp-file "night-olpl-warm" t))
         (texfile (expand-file-name "chunk.tex" tmpdir))
         (bg (plist-get info :bg)))
    (with-temp-file texfile
      (insert (plist-get info :header))
      (insert "\n\\begin{document}\n"
              "\\definecolor{fg}{rgb}{" (plist-get info :fg) "}%\n"
              (if bg
                  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
                          "\n\\pagecolor{bg}%\n")
                ""))
      (let ((first t))
        (dolist (task chunk)
          (unless first (insert "\n\\newpage\n"))
          (setq first nil)
          (insert "\n{\\color{fg}\n"
                  (night/h-olpl-warm-body (plist-get task :value))
                  "\n}\n")))
      (insert "\n\\end{document}\n"))
    ;; Mirrors the :latex-compiler template of the dvisvgm entry in
    ;; `org-preview-latex-process-alist' (org 9.7), niced.
    ;; :buffer nil + pipe: latex's chatty nonstopmode console output
    ;; must be discarded, not collected — with 8 concurrent pipelines
    ;; on default PTYs (~16KB buffers), the children BLOCK on writes
    ;; whenever Emacs doesn't drain fast enough (observed: 25-45% CPU
    ;; and a ~25x wall-time blowup). Diagnostics live on in the
    ;; tmpdir's .log file.
    (let ((proc (make-process
                 :name "night-olpl-warm-latex"
                 :buffer nil
                 :connection-type 'pipe
                 :command (list "nice" "-n" "10" "latex"
                                "-interaction" "nonstopmode"
                                "-output-directory" tmpdir texfile)
                 :noquery t
                 :sentinel #'night/h-olpl-warm-latex-sentinel)))
      (process-put proc 'olpl-chunk chunk)
      (process-put proc 'olpl-tmpdir tmpdir)
      (process-put proc 'olpl-buf (current-buffer))
      (process-put proc 'olpl-solo solo-retry-p)
      (process-put proc 'olpl-scale (plist-get info :scale))
      (push proc night/h-olpl-warm-procs))))

(defun night/h-olpl-warm-latex-sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (let* ((buf (process-get proc 'olpl-buf))
           (tmpdir (process-get proc 'olpl-tmpdir))
           (dvifile (expand-file-name "chunk.dvi" tmpdir)))
      (cond
       ((not (buffer-live-p buf))
        (night/h-olpl-warm-orphan-cleanup proc))
       (t
        (with-current-buffer buf
          (when (memq proc night/h-olpl-warm-procs) ;; not cancelled
            (cond
             ;; nonstopmode usually recovers and still emits a DVI even
             ;; on errors; the page-count check downstream is the real
             ;; validation. No DVI at all = hard failure.
             ((not (file-exists-p dvifile))
              (night/h-olpl-warm-chunk-failed proc))
             (t
              ;; Mirrors the :image-converter template of the dvisvgm
              ;; entry, extended for multi-page output.
              (let ((dproc (make-process
                            :name "night-olpl-warm-dvisvgm"
                            :buffer nil
                            :connection-type 'pipe
                            :command (list "nice" "-n" "10" "dvisvgm" dvifile
                                           "--no-fonts" "--exact-bbox"
                                           (format "--scale=%s"
                                                   (process-get proc 'olpl-scale))
                                           "--page=1-"
                                           (format "--output=%s"
                                                   (expand-file-name
                                                    "out-%p.svg" tmpdir)))
                            :noquery t
                            :sentinel #'night/h-olpl-warm-dvisvgm-sentinel)))
                (dolist (k '(olpl-chunk olpl-tmpdir olpl-buf olpl-solo olpl-scale))
                  (process-put dproc k (process-get proc k)))
                (setq night/h-olpl-warm-procs
                      (cons dproc (delq proc night/h-olpl-warm-procs)))))))))))))

(defun night/h-olpl-warm-dvisvgm-sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (let* ((buf (process-get proc 'olpl-buf))
           (tmpdir (process-get proc 'olpl-tmpdir))
           (chunk (process-get proc 'olpl-chunk)))
      (cond
       ((not (buffer-live-p buf))
        (night/h-olpl-warm-orphan-cleanup proc))
       (t
        (with-current-buffer buf
          (when (memq proc night/h-olpl-warm-procs) ;; not cancelled
            ;; dvisvgm ZERO-PADS %p when the document has >= 10 pages
            ;; (out-01.svg...), so never predict the names — glob and
            ;; sort numerically by page number.
            (let* ((outs (sort (directory-files
                                tmpdir t "\\`out-[0-9]+\\.svg\\'")
                               (lambda (a b)
                                 (< (night/h-olpl-warm-out-page a)
                                    (night/h-olpl-warm-out-page b))))))
              (cond
               ;; Page count must match the chunk — a broken fragment
               ;; can shift or swallow pages.
               ((/= (length outs) (length chunk))
                (night/h-olpl-warm-chunk-failed proc))
               (t
                (cl-loop for task in chunk
                         for out in outs
                         do (let* ((tofile (plist-get task :tofile))
                                   (tmp-target (format "%s.tmp%d" tofile (emacs-pid))))
                              (make-directory (file-name-directory tofile) t)
                              ;; copy + rename: the rename is atomic, so
                              ;; a reader never sees a partial file.
                              (copy-file out tmp-target t)
                              (rename-file tmp-target tofile t)))
                (setq night/h-olpl-warm-procs (delq proc night/h-olpl-warm-procs))
                (ignore-errors (delete-directory tmpdir t))
                (cl-incf night/h-olpl-warm-done-chunks)
                (cl-incf night/h-olpl-warm-done-frags (length chunk))
                (cond
                 ;; bg: render this chunk's fragments RIGHT NOW —
                 ;; event-driven, no tick involved. Bounded work:
                 ;; ~1ms per cache-hit render.
                 ((eq night/org-latex-preview-lazy-mode 'bg)
                  (dolist (task chunk)
                    (night/h-olpl-warm-render-task task))
                  (message "night/org-latex-preview-lazy: %d/%d fragments warmed (chunk %d/%d)"
                           night/h-olpl-warm-done-frags
                           night/h-olpl-warm-total-frags
                           night/h-olpl-warm-done-chunks
                           night/h-olpl-warm-total-chunks))
                 (t
                  ;; timer+bg: the drain renders; just unregister and
                  ;; wake it to sweep the new cache hits.
                  (dolist (task chunk)
                    (night/h-olpl-warm-free-task task))
                  (message "night/org-latex-preview-lazy: %d/%d fragments warmed (chunk %d/%d)"
                           night/h-olpl-warm-done-frags
                           night/h-olpl-warm-total-frags
                           night/h-olpl-warm-done-chunks
                           night/h-olpl-warm-total-chunks)
                  (night/h-olpl-request-arm buf)))
                (night/h-olpl-warm-dispatch-next)))))))))))

(defun night/h-olpl-warm-render-task (task)
  "Render TASK's fragments from its fresh cache file, then free it.
Re-validates every fragment against the CURRENT buffer state: dead
markers, fragments edited mid-compile (their hash no longer matches
the produced file), already-previewed ones, and the fragment under
point (org-fragtog previews it as a cache hit on exit) all render
nothing."
  (let ((tofile (plist-get task :tofile)))
    (dolist (frag (plist-get task :markers))
      (let ((beg (marker-position (car frag)))
            (end (marker-position (cdr frag))))
        (when (and beg end
                   (not (night/h-olpl-previewed-p beg))
                   (not (and (>= (point) beg) (< (point) end))))
          (let ((context (save-excursion (goto-char beg)
                                         (org-element-context))))
            (when (and (memq (org-element-type context)
                             '(latex-fragment latex-environment))
                       (equal (night/h-olpl-cache-file
                               (org-element-property :value context) beg)
                              tofile))
              (night/h-olpl-render-cached beg end tofile))))))
    (night/h-olpl-warm-free-task task)))

(defun night/h-olpl-warm-out-page (file)
  "Page number encoded in a dvisvgm out-N.svg FILE name."
  (if (string-match "out-0*\\([0-9]+\\)\\.svg\\'" file)
      (string-to-number (match-string 1 file))
    0))

(defun night/h-olpl-warm-chunk-failed (proc)
  "Handle a failed chunk: retry per-fragment, or report+drop when solo."
  (let ((chunk (process-get proc 'olpl-chunk))
        (solo (process-get proc 'olpl-solo))
        (tmpdir (process-get proc 'olpl-tmpdir)))
    (setq night/h-olpl-warm-procs (delq proc night/h-olpl-warm-procs))
    (ignore-errors (delete-directory tmpdir t))
    (cond
     (solo
      ;; Permanent failure: unregister; nothing renders it.
      (cl-incf night/h-olpl-warm-failed)
      (cl-incf night/h-olpl-warm-done-chunks)
      (message "night/org-latex-preview-lazy: background compile failed for: %.70s"
               (plist-get (car chunk) :value))
      (dolist (task chunk)
        (night/h-olpl-warm-free-task task)))
     (t
      ;; Isolate the broken fragment: requeue each one as its own chunk.
      (cl-incf night/h-olpl-warm-total-chunks (1- (length chunk)))
      (dolist (task chunk)
        (push (cons t (list task)) night/h-olpl-warm-pending))))
    (night/h-olpl-warm-dispatch-next)))

(defun night/h-olpl-warm-orphan-cleanup (proc)
  "Cleanup for a pipeline whose buffer died.
The buffer-local task index died with the buffer; only markers and the
temp dir need freeing."
  (dolist (task (process-get proc 'olpl-chunk))
    (dolist (frag (plist-get task :markers))
      (set-marker (car frag) nil)
      (set-marker (cdr frag) nil)))
  (let ((tmpdir (process-get proc 'olpl-tmpdir)))
    (when tmpdir (ignore-errors (delete-directory tmpdir t)))))

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
   ((or (eq night/org-latex-preview-lazy-mode 'original) ;; kill switch
        night/h-olpl-inhibit-reroute ;; our own re-entrant renders/compiles
        (night/org-latex-preview-new-system-p)
        (not (display-graphic-p)))
    (funcall orig-fn beg end))
   ;; bg: fully event-driven — no queue, no timers (see
   ;; `night/h-olpl-bg-dispatch').
   ((eq night/org-latex-preview-lazy-mode 'bg)
    (night/h-olpl-bg-dispatch beg end))
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
       ;; Unified sync rule: cached fragments render at ~0.25ms each,
       ;; so a mostly-cached region with at most sync-threshold COLD
       ;; fragments costs no more than a threshold-sized region would —
       ;; render it synchronously ("cached => instant", tolerating the
       ;; usual cold budget). Subsumes both "count <= threshold" (then
       ;; uncached <= threshold trivially) and "all cached" (uncached =
       ;; 0). The count cap is checked BEFORE any hashing so it bounds
       ;; the check itself too.
       ((and (<= count night/org-latex-preview-lazy-sync-cached-max)
             (<= (night/h-olpl-uncached-count
                  frags (1+ night/org-latex-preview-lazy-sync-threshold))
                 night/org-latex-preview-lazy-sync-threshold))
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
      (when (and (not (eq night/org-latex-preview-lazy-mode 'original))
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

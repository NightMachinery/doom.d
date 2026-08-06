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

(defun night/h-olpl-fragments (&optional beg end)
  "Collect LaTeX fragments/environments as marker-pair conses.
With BEG and END, scan only that region; callers must pass
element-aligned bounds (see `night/h-olpl-element-bound')."
  (save-restriction
    (when (and beg end)
      (narrow-to-region beg end))
    (org-element-map (org-element-parse-buffer)
        '(latex-fragment latex-environment)
      (lambda (el)
        (cons (copy-marker (org-element-property :begin el))
              (copy-marker (org-element-property :end el)))))))

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

;;; Buffer-modification watching: fragments pasted or typed AFTER the
;;; initial scan would otherwise never render (the queue is a one-shot
;;; snapshot, and a finished drain disarms itself). Each edit widens a
;;; dirty region (O(1) marker updates); the next tick rescans just that
;;; region and merges any new fragments into the queue.
(defvar-local night/h-olpl-dirty-beg nil
  "Marker at the start of the region edited since the last scan, or nil.")

(defvar-local night/h-olpl-dirty-end nil
  "Marker at the end of the region edited since the last scan, or nil.")

(defvar-local night/h-olpl-watching nil
  "Whether this buffer's modification watcher is installed.")

(defun night/h-olpl-dirty-clear ()
  (when night/h-olpl-dirty-beg (set-marker night/h-olpl-dirty-beg nil))
  (when night/h-olpl-dirty-end (set-marker night/h-olpl-dirty-end nil))
  (setq night/h-olpl-dirty-beg nil
        night/h-olpl-dirty-end nil))

(defun night/h-olpl-watch ()
  "Install the buffer-modification watcher (idempotent)."
  (unless night/h-olpl-watching
    (setq night/h-olpl-watching t)
    (add-hook 'after-change-functions #'night/h-olpl-after-change nil t)))

(defun night/h-olpl-unwatch ()
  "Remove the buffer-modification watcher and forget pending edits."
  (setq night/h-olpl-watching nil)
  (remove-hook 'after-change-functions #'night/h-olpl-after-change t)
  (night/h-olpl-dirty-clear))

(defun night/h-olpl-after-change (beg end _len)
  "Widen the dirty region to cover BEG..END. Runs on every edit; O(1)."
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
              (org--latex-preview-region beg end)
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
            ;; Stay quiet on ticks that only absorbed non-LaTeX edits.
            (when worked
              (message "night/org-latex-preview-lazy: all previews done"))
            ;; Keep the watcher: future pastes/edits must still arm us.
            (night/org-latex-preview-lazy-stop 'keep-watch)))))))))

(defun night/org-latex-preview-lazy-stop (&optional keep-watch)
  "Cancel lazy previewing in the current buffer, freeing all state.
With KEEP-WATCH non-nil (used on normal drain completion), the
buffer-modification watcher stays installed so later pastes/edits still
get previewed; interactively the watcher is removed too."
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
  (unless keep-watch
    (night/h-olpl-unwatch)))

(defun night/h-olpl-usable-p ()
  "Whether the lazy machinery can run, signaling `user-error' when not."
  (cond
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
    (night/h-olpl-watch)
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
    (night/h-olpl-watch)
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
;; Make lazy previewing the DEFAULT for every multi-fragment preview path:
;; - `#+STARTUP: latexpreview' / `C-u C-u' (whole buffer): org.el calls
;;   `(org-latex-preview '(16))' during `org-mode' initialization, which
;;   would freeze Emacs before the user can intervene;
;; - an active region (unbounded size);
;; - no prefix with point NOT on a fragment: org renders the whole
;;   SECTION synchronously. org-fragtog's fragment-exit handler calls
;;   no-arg `org-latex-preview' and can land in this branch when its
;;   stale parse misses the fragment (e.g. affiliated keywords), turning
;;   a cursor motion into a section-wide freeze.
;; Toggling the single fragment at point stays synchronous, as do the
;; clearing paths (`C-u', `C-u C-u C-u') — they are cheap.
(defun night/h-olpl-around-org-latex-preview (orig-fn &optional arg)
  (cond
   ((or (night/org-latex-preview-new-system-p)
        (not (fboundp 'org--latex-preview-region))
        (not (display-graphic-p))
        (and (bound-and-true-p untrusted-content)
             (not (bound-and-true-p org--latex-preview-when-risky)))
        (member arg '((4) (64))))
    (funcall orig-fn arg))
   ((equal arg '(16))
    (night/org-latex-preview-lazy))
   ((use-region-p)
    (night/org-latex-preview-lazy-region (region-beginning) (region-end)))
   ((memq (org-element-type (org-element-context))
          '(latex-fragment latex-environment))
    (funcall orig-fn arg))
   (t
    (let ((beg (if (org-before-first-heading-p) (point-min)
                 (save-excursion
                   (org-with-limited-levels (org-back-to-heading t))
                   (point))))
          (end (org-with-limited-levels (org-entry-end-position))))
      (night/org-latex-preview-lazy-region beg end)))))

(after! org
  (advice-add #'org-latex-preview
              :around #'night/h-olpl-around-org-latex-preview))

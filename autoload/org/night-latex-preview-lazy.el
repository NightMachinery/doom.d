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
      (when (and (buffer-live-p buf)
                 (buffer-local-value 'night/h-olpl-queue buf))
        (with-current-buffer buf
          (night/h-olpl-schedule buf))))))

(defun night/h-olpl-fragments ()
  "Collect all LaTeX fragments/environments as marker-pair conses."
  (org-element-map (org-element-parse-buffer)
      '(latex-fragment latex-environment)
    (lambda (el)
      (cons (copy-marker (org-element-property :begin el))
            (copy-marker (org-element-property :end el))))))

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
        (let ((win (get-buffer-window buf)))
          (when win
            (with-selected-window win
              (night/h-olpl-resort))))
        ;; Work until the tick's time budget is spent: cache hits and
        ;; skips cost ~1ms each and flow through in bulk, cold compiles
        ;; cap the tick at about one fragment.
        (let ((deadline (+ (float-time) night/org-latex-preview-lazy-tick-seconds)))
          (while (and night/h-olpl-queue (< (float-time) deadline))
            (night/h-olpl-preview-1 (pop night/h-olpl-queue))))
        (cond
         (night/h-olpl-queue (night/h-olpl-schedule buf 'resting))
         (t
          (message "night/org-latex-preview-lazy: all previews done")
          (night/org-latex-preview-lazy-stop))))))))

(defun night/org-latex-preview-lazy-stop ()
  "Cancel lazy previewing in the current buffer, freeing all state."
  (interactive)
  (setq night/h-olpl-pending-buffers
        (delq (current-buffer) night/h-olpl-pending-buffers))
  (when night/h-olpl-timer
    (cancel-timer night/h-olpl-timer)
    (setq night/h-olpl-timer nil))
  (dolist (frag night/h-olpl-queue)
    (set-marker (car frag) nil)
    (set-marker (cdr frag) nil))
  (setq night/h-olpl-queue nil))

(defun night/org-latex-preview-lazy ()
  "Preview all LaTeX fragments progressively without freezing Emacs.

Fragments visible in the window are compiled first (one chunk
immediately), the rest from idle timers in chunks of
`night/org-latex-preview-lazy-chunk-size'. Scrolling re-prioritizes the
queue towards the viewport. Stop with
`night/org-latex-preview-lazy-stop'."
  (interactive)
  (cond
   ((not (derived-mode-p 'org-mode))
    (user-error "night/org-latex-preview-lazy: not an org buffer"))
   ((night/org-latex-preview-new-system-p)
    (user-error
     "The new async org-latex-preview system is installed; use it directly"))
   ((not (fboundp 'org--latex-preview-region))
    (user-error
     "`org--latex-preview-region' is missing; org internals have changed"))
   (t
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
      (night/h-olpl-request-arm (current-buffer)))))))

;;;
;; Make lazy previewing the DEFAULT for whole-buffer previews: both
;; `#+STARTUP: latexpreview' (org.el calls `(org-latex-preview '(16))'
;; during `org-mode' initialization, which would freeze Emacs before the
;; user can intervene) and interactive `C-u C-u org-latex-preview'.
;; Section-level previews (no prefix) stay synchronous: they are small.
(defun night/h-olpl-around-org-latex-preview (orig-fn &optional arg)
  (cond
   ((and (equal arg '(16))
         (not (night/org-latex-preview-new-system-p))
         (fboundp 'org--latex-preview-region))
    (night/org-latex-preview-lazy))
   (t (funcall orig-fn arg))))

(after! org
  (advice-add #'org-latex-preview
              :around #'night/h-olpl-around-org-latex-preview))

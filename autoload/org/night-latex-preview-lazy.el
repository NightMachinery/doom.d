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
(defvar night/org-latex-preview-lazy-chunk-size 2
  "How many fragments to compile per tick.
Each fragment blocks Emacs for roughly 0.3-1s, so this bounds the pause
length between which Emacs stays responsive.")

(defvar night/org-latex-preview-lazy-idle-delay 0.5
  "Idle seconds to wait before compiling the next chunk.")

(defvar-local night/h-olpl-queue nil
  "Pending fragments, a list of (BEGIN-MARKER . END-MARKER) conses.")

(defvar-local night/h-olpl-timer nil
  "The scheduled idle timer for the next chunk, if any.")

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
Frees the markers afterwards; errors are reported but do not abort the
queue."
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
                      beg (error-message-string err))))))
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

(defun night/h-olpl-on-scroll (win _start)
  "Re-prioritize the queue of WIN's buffer towards the new viewport."
  (let ((buf (window-buffer win)))
    (when (buffer-local-value 'night/h-olpl-queue buf)
      (with-current-buffer buf
        (with-selected-window win
          (night/h-olpl-resort))))))

(defun night/h-olpl-schedule (buf)
  "Schedule the next tick for BUF.
While Emacs stays idle, chunks continue back-to-back (idle timers only
fire once per idle period, so the continuation must extend the current
idle time); once the user is active again, wait for the next idle
period."
  (let ((idle (current-idle-time)))
    (setq night/h-olpl-timer
          (run-with-idle-timer
           (cond
            (idle (time-add idle night/org-latex-preview-lazy-idle-delay))
            (t night/org-latex-preview-lazy-idle-delay))
           nil #'night/h-olpl-tick buf))))

(defun night/h-olpl-tick (buf)
  "Compile one chunk of BUF's queue, then reschedule or finish."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq night/h-olpl-timer nil)
      (let ((n night/org-latex-preview-lazy-chunk-size))
        (while (and night/h-olpl-queue (> n 0))
          (night/h-olpl-preview-1 (pop night/h-olpl-queue))
          (setq n (1- n))))
      (cond
       (night/h-olpl-queue (night/h-olpl-schedule buf))
       (t
        (message "night/org-latex-preview-lazy: all previews done")
        (night/org-latex-preview-lazy-stop))))))

(defun night/org-latex-preview-lazy-stop ()
  "Cancel lazy previewing in the current buffer, freeing all state."
  (interactive)
  (when night/h-olpl-timer
    (cancel-timer night/h-olpl-timer)
    (setq night/h-olpl-timer nil))
  (dolist (frag night/h-olpl-queue)
    (set-marker (car frag) nil)
    (set-marker (cdr frag) nil))
  (setq night/h-olpl-queue nil)
  (remove-hook 'window-scroll-functions #'night/h-olpl-on-scroll t))

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
      (night/h-olpl-resort)
      (add-hook 'window-scroll-functions #'night/h-olpl-on-scroll nil t)
      (message "night/org-latex-preview-lazy: previewing %d fragments ..."
               (length night/h-olpl-queue))
      (night/h-olpl-tick (current-buffer)))))))

;;; autoload/night-minibuffer.el -*- lexical-binding: t; -*-
;;;
;;; Keeping the minibuffer from becoming a trap.
;;;
;;; Symptom: a terminal frame opens, focus sits in the minibuffer, and almost
;;; every key is rejected.  It looks like a broken terminal or a broken keymap;
;;; it is neither.
;;;
;;; The mechanism, after independent review corrected an earlier guess of mine:
;;; Emacs 29.2 leaks minibuffer depth.  `read_minibuf' increments minibuf_level,
;;; then `temporarily_switch_to_single_kboard' can signal "Terminal N is locked,
;;; cannot read from it" -- which means keyboard arbitration, not a dead
;;; terminal -- *before* `read_minibuf_unwind' is registered.  The increment is
;;; then left with no cleanup.  `recursion-depth' is command_loop_level plus
;;; minibuf_level, so it climbs and never falls, later frames open inside the
;;; count, and no Lisp call repairs it: on a wedged daemon, `top-level',
;;; `abort-recursive-edit' and deleting the owning frames all left it untouched.
;;;
;;; Two claims that were in this file and are NOT established: that a modified
;;; ` *temp*' buffer survives `with-temp-buffer' (stock 29.2 kills it, since the
;;; save confirmation is conditional on an interactive call), and that
;;; `save-some-buffers' explains the depth (`map-y-or-n-p' uses `read-event' in
;;; the echo area and never enters `read_minibuf').  The phantom buffer was
;;; real and worth removing; it was not the cause of the depth.
;;;
;;; @see ~/scripts/docs/emacs-minibuffer-wedge.md

;;; Show the nesting depth.  `enable-recursive-minibuffers' is t here, which
;;; ivy relies on, so prompts legitimately stack -- but with no indicator the
;;; depth is invisible, and ESC (`abort-recursive-edit', one level per press)
;;; looks inert when several presses are needed.  This puts [N] in the prompt,
;;; so the depth is legible and each press visibly counts down.
(minibuffer-depth-indicate-mode 1)

(defun night/h-save-some-buffers-worth-prompting-p ()
  "Should `save-some-buffers' ask about the current buffer?

Internal buffers -- the leading-space convention -- are never the user's
to save.  When one is modified and carries a `buffer-file-name', which
only happens because some code set it by hand, `save-some-buffers' would
otherwise prompt for it forever, and that prompt rejects nearly every
key.  Skip them; every ordinary buffer is still offered."
  (not (string-prefix-p " " (buffer-name))))

(setq save-some-buffers-default-predicate
      #'night/h-save-some-buffers-worth-prompting-p)

(defun night/minibuffer-diagnose ()
  "Report why a frame might be stuck in the minibuffer, and offer to clean.

Deliberately does not unwind anything.  A depth that will not fall is
leaked C state, and no Lisp call recreates the missing cleanup record;
`top-level' would only end live client sessions without repairing it.
Note that `server-goto-toplevel' in server.el does call `top-level' when
a minibuffer is active, so calling it is not itself unreasonable -- it
just does not fix this."
  (interactive)
  (let* ((clients (bound-and-true-p server-clients))
         (phantoms (seq-filter
                    (lambda (b)
                      (and (buffer-modified-p b)
                           (buffer-file-name b)
                           (string-prefix-p " " (buffer-name b))))
                    (buffer-list)))
         ;; A client with no frame at all is usually the `emacsclient -e'
         ;; asking this very question, not an orphan.  Only a client whose
         ;; frame existed and has since died is one.
         (orphans (seq-filter
                   (lambda (c)
                     (let ((frame (process-get c 'frame)))
                       (and frame (not (frame-live-p frame)))))
                   clients)))
    (message "recursion-depth=%d minibuffer-depth=%d clients=%d orphaned-clients=%d phantom-buffers=%d%s"
             (recursion-depth) (minibuffer-depth)
             (length clients) (length orphans) (length phantoms)
             (cond
              (phantoms (format " => %s" (mapcar #'buffer-name phantoms)))
              (t "")))
    (when (and phantoms
               (y-or-n-p (format "Kill %d phantom buffer(s)? " (length phantoms))))
      (dolist (b phantoms)
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b))
      (message "Killed %d phantom buffer(s)." (length phantoms)))))

;;; Early warning.
;;;
;;; This cannot repair anything, and does not try.  The depth that will not fall
;;; is leaked C state (see the header): no Lisp call recreates the cleanup record
;;; that was never registered, so restarting the daemon is the only exit once it
;;; has happened.  What is achievable is making the state legible at the one
;;; moment it matters -- as a new frame opens inside the count -- so the answer
;;; is "look at the depth" rather than a frame that ignores the keyboard.
;;;
;;; It reports rather than diagnoses, on purpose.  `active-minibuffer-window'
;;; falls back to `minibuf_window' when it cannot find the buffer for the
;;; current level displayed anywhere, so with leaked levels the frame it names
;;; may be that fallback and not a real prompt owner.  Whether that frame has a
;;; tty is therefore a hint about where to look, not proof of anything.

(defun night/h-minibuffer-owner ()
  "Return the frame whose minibuffer is currently active, or nil."
  (let ((window (active-minibuffer-window)))
    (cond
     (window (window-frame window))
     (t nil))))

(defun night/h-minibuffer-nesting-message (depth name tty)
  "Describe opening a frame inside DEPTH minibuffer levels, or return nil.

NAME and TTY belong to the frame `active-minibuffer-window' points at.
Kept free of Emacs state so both branches are testable by calling it,
rather than by stubbing primitives.

A non-nil TTY means there is a frame you can type at, so the likely move
is to answer or abort the prompt there.  A nil one is only a hint: it can
mean a real prompt on the daemon's own frame, or the fallback that
`active-minibuffer-window' returns when no live window shows the level,
which is what leaked depth looks like.  Neither case is asserted to be
recoverable or not."
  (cond
   ((<= depth 0) nil)
   ((null tty)
    (format "Emacs: %d minibuffer level(s) active, owner has no tty -- may be a real prompt or leaked depth.  If keys do nothing, restart the daemon.  M-x night/minibuffer-diagnose"
            depth))
   (t
    (format "Emacs: opened inside %d active minibuffer level(s), owner %s on %s -- try answering or aborting it there"
            depth name tty))))

(defun night/h-minibuffer-warn-if-nested ()
  "Say so when this frame has opened inside active minibuffer levels."
  (let* ((owner (night/h-minibuffer-owner))
         (text (night/h-minibuffer-nesting-message
                (minibuffer-depth)
                (and owner (frame-parameter owner 'name))
                (and owner (frame-parameter owner 'tty)))))
    (cond
     (text (message "%s" text))
     (t nil))))

(defun night/h-minibuffer-warn-after-make-frame ()
  "Schedule `night/h-minibuffer-warn-if-nested' once the new frame settles.

Deferred because a message logged during frame setup is promptly buried
by whatever the frame draws next."
  (run-with-timer 0.5 nil #'night/h-minibuffer-warn-if-nested))

(add-hook 'server-after-make-frame-hook
          #'night/h-minibuffer-warn-after-make-frame)

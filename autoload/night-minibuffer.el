;;; autoload/night-minibuffer.el -*- lexical-binding: t; -*-
;;;
;;; Keeping the minibuffer from becoming a trap.
;;;
;;; Symptom this exists for: a terminal frame opens, focus sits in the
;;; minibuffer, and almost every key is rejected.  It looks like a broken
;;; terminal or a broken keymap; it is neither.  It is `save-some-buffers'
;;; prompting through `map-y-or-n-p', whose keymap answers anything outside
;;; (y n ! . q C-r C-f d C-h) with "Type C-h for help." -- ESC included.
;;;
;;; The instance that prompted this had a ` *temp*' buffer left modified while
;;; visiting /Users/fixture/code/x.py, a path that does not and cannot exist,
;;; from a session testing fill-in-middle (see ~/scripts/docs/fim.md).
;;; `with-temp-buffer' ends in `kill-buffer', which does not kill a modified
;;; file-visiting buffer -- it asks -- so the buffer outlived the test and
;;; every later `save-some-buffers' asked about it again.
;;;
;;; @see ~/scripts/docs/tmux-termux-truecolor.md for the unrelated terminal
;;; work this was first mistaken for.
;;;

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

Deliberately does not unwind recursive edits on its own.  In a daemon a
waiting `emacsclient -t' client legitimately holds one each -- that is
what \"When done with this frame, type SPC q f\" means -- so `top-level'
would end other people's live sessions, not just the orphaned levels."
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
;;; A daemon can reach a state no code can repair.  Once a terminal dies while
;;; one of its minibuffer reads is live, Emacs reports "Terminal N is locked,
;;; cannot read from it" and that level is stranded on a command loop nothing
;;; can drive: measured on a wedged daemon, `top-level' ran twice and the depth
;;; stayed at 8, ten `abort-recursive-edit' calls changed nothing, and deleting
;;; the owning frames only migrated the stack to the daemon's own frame, which
;;; has no tty to type into.  Restarting is then the only way out.
;;;
;;; So this cannot fix anything, and does not try.  It makes the state legible
;;; at the one moment it matters -- when a new frame is about to open inside
;;; the pile -- so the answer is "restart the daemon" rather than a frame that
;;; mysteriously ignores the keyboard.

(defun night/h-minibuffer-owner ()
  "Return the frame whose minibuffer is currently active, or nil."
  (let ((window (active-minibuffer-window)))
    (cond
     (window (window-frame window))
     (t nil))))

(defun night/h-minibuffer-nesting-message (depth name tty)
  "Describe opening a frame inside DEPTH minibuffer levels, or return nil.

NAME and TTY belong to the frame owning the active minibuffer.  Kept
free of Emacs state so both branches are testable by calling it, rather
than by stubbing primitives.

A non-nil TTY means somebody's live prompt, answerable where it sits.  A
nil one means nothing can reach that command loop any more."
  (cond
   ((<= depth 0) nil)
   ((null tty)
    (format "Emacs: %d minibuffer level(s) active with no typeable owner -- this daemon is wedged; restart it.  M-x night/minibuffer-diagnose"
            depth))
   (t
    (format "Emacs: opened inside %d active minibuffer level(s), owned by %s on %s -- answer or abort it there"
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

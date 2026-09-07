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
         (orphans (seq-filter
                   (lambda (c)
                     (let ((frame (process-get c 'frame)))
                       (not (and frame (frame-live-p frame)))))
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

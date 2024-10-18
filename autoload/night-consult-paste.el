;;; autoload/night-consult-paste.el -*- lexical-binding: t; -*-
;;;
;; @o1
;;
;; * Known Bugs:
;; - Scrolling the other buffer during candidate selection is buggy.
;; The original [help:consult-yank-from-kill-ring] also has this bug.
;;;
(after! (night-consult)
  (defun night/replace-preview (beg end)
    "Create a preview function that replaces text between BEG and END."
    (let ((ov (make-overlay beg end)))
      (lambda (action cand)
        (pcase action
          ('setup
           ;; Initialize the overlay properties
           (overlay-put ov 'face 'consult-preview-insertion))
          ('preview
           (if cand
               (overlay-put ov 'display cand)
             (overlay-put ov 'display nil)))
          ('exit
           ;; Always delete the overlay when the command ends
           (delete-overlay ov))))))

  (defun night/consult--read-from-kill-ring ()
    "Open kill ring menu and return selected string, with proper preview."
    ;; Ensure `kill-ring' is up to date with any interprogram paste
    (current-kill 0)
    ;; Determine the appropriate preview function
    (let ((state-fn
           (if (and (region-active-p) evil-kill-on-visual-paste)
               ;; Use `night/replace-preview` when replacing a selected region
               (night/replace-preview (region-beginning) (region-end))
             ;; Otherwise, use the standard insertion preview
             (consult--insertion-preview
              (point)
              ;; If the previous command was `yank`, extend the preview to the mark
              (if (eq last-command 'yank) (mark t) (point))))))
      ;; Perform the `consult--read` with the chosen preview function
      (consult--lookup-member
       (consult--read
        (consult--remove-dups
         (or (if consult-yank-rotate
                 (append kill-ring-yank-pointer
                         (butlast kill-ring (length kill-ring-yank-pointer)))
               kill-ring)
             (user-error "Kill ring is empty")))
        :prompt "Yank from kill-ring: "
        :history t ;; Disable history
        :sort nil
        :category 'kill-ring
        :state state-fn)
       kill-ring)))

  (defun night/consult-yank-from-kill-ring (string &optional arg)
    "Select STRING from the kill ring and insert it.
With prefix ARG, put point at beginning, and mark at end, like `yank' does.

This command behaves like `consult-yank-from-kill-ring', but respects
`evil-kill-on-visual-paste' and kills the selected region if necessary."
    (interactive (list (night/consult--read-from-kill-ring) current-prefix-arg))
    (when string
      ;; Kill the selected region if `evil-kill-on-visual-paste` is enabled
      (when (and (region-active-p) evil-kill-on-visual-paste)
        (kill-region (region-beginning) (region-end)))
      ;; Proceed with the original yank operation
      (setq yank-window-start (window-start))
      (push-mark)
      (insert-for-yank string)
      (setq this-command 'yank)
      (when consult-yank-rotate
        (if-let ((pos (seq-position kill-ring string)))
            (setq kill-ring-yank-pointer (nthcdr pos kill-ring))
          (kill-new string)))
      (when (consp arg)
        ;; Swap point and mark like in `yank'
        (goto-char (prog1 (mark t)
                     (set-marker (mark-marker) (point) (current-buffer)))))))
  )

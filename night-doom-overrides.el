(after! smartparens (smartparens-global-mode -1))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;;;
(defun doom-quit-p (&optional prompt)
  "Prompt the user for confirmation when killing Emacs.

Returns t if it is safe to kill this session. Does not prompt if no real buffers
are open."
  (or 't (not (ignore-errors (doom-real-buffer-list)))
      (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
      (ignore (message "Aborted"))))
;;;
(general-with-eval-after-load "snippets"
  ;;; Useful for org files. They start with a snippet from the beginning which is very unintuitive.
  (defun +snippets/goto-start-of-field ()
    "Go to the beginning of the current field."
    (interactive)
    (evil-beginning-of-line))

  (defun +snippets/goto-end-of-field ()
    "Go to the end of the current field."
    (interactive)
    (evil-end-of-line)))
;;;
(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let ((old-path (buffer-file-name (buffer-base-buffer)))
        (new-path (expand-file-name new-path)))
    (when (directory-name-p new-path)
      (setq new-path (concat new-path (f-filename old-path)))
      )
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)
    (doom--update-files old-path new-path)
    (message "File moved to %S" (abbreviate-file-name new-path))))
;;;

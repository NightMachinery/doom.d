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

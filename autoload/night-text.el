;;; ~/doom.d/night-text.el -*- lexical-binding: t; -*-
;;;
(defun night/copy-buffer-content ()
  "Copy the entire content of the current buffer to the clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Buffer content copied to clipboard!"))
;;;
(defun night/current-line-select ()
  (interactive)
  (move-beginning-of-line nil)
  (set-mark-command nil)
  (move-end-of-line nil)
  (setq deactivate-mark nil))
(defalias 'my-select-current-line #'night/current-line-select)

(defun night/current-line-get ()
  "Get the current line as a string."
  (buffer-substring-no-properties
   (line-beginning-position)
   (line-end-position)))

(defun night/current-line-starts-with-p (prefix)
  "Check if the current line starts with the given PREFIX.
Return t if the line starts with PREFIX, nil otherwise."
  (let ((line (night/current-line-get)))
    (string-prefix-p prefix line)))
;;;
(defun kill-all-comments ()
  "Kills all the comments in the code, without putting them in the killring."
  (interactive)
  (goto-char (point-min))
  (let (kill-ring)
    (comment-kill (count-lines (point-min) (point-max)))))
(defun escape-doublequotes-at-car-of-kill-ring ()
  "Escape doublequotes in car of kill-ring "
  (interactive)
  (with-temp-buffer
    (insert (car kill-ring))
    (goto-char (point-min))
    (while (search-forward "\"" nil t 1)
      (replace-match "\\\\\""))
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))))
;;;
(defun night/erase-ansi (str)
  "Remove ANSI escape sequences from STR."
  (if (stringp str)
      (progn
        ;; (message "erase-ansi before: %s" str)
        (let ((result (replace-regexp-in-string "\033\\(\\[[0-9;]*\\([a-zA-Z]\\)?\\)?" "" str)))
          ;; (message "erase-ansi after: %s" result)
          result))
    (error "night/erase-ansi: expected a string")))
;;;

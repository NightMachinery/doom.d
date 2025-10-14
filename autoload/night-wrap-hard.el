;;; autoload/night-wrap-hard.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(defcustom night/hard-wrap-default-column 60
  "Default column to wrap at when `:column` is not provided."
  :type 'integer
  :group 'night)

(defcustom night/hard-wrap-before-hook nil
  "Hook run before hard-wrapping. Called with no args."
  :type 'hook
  :group 'night)

(defcustom night/hard-wrap-after-hook nil
  "Hook run after hard-wrapping. Called with no args."
  :type 'hook
  :group 'night)

(cl-defun night/hard-wrap-buffer (&key column justify filler)
  "Hard-wrap the current buffer, or the active region if any.

When a region is active, wrap only that region; otherwise wrap the whole
buffer. The operation uses `fill-region' and respects `fill-prefix',
`adaptive-fill-mode', and mode-specific fill behavior.

Keyword args:
- :column  Wrap width (integer). Defaults to `night/hard-wrap-default-column'.
- :justify When non-nil, fully justify each line (passed to `fill-region').
- :filler  Function of signature like `fill-region' to perform wrapping.
           Defaults to `fill-region'. This allows dependency injection.

Interactively:
- With no prefix, uses `night/hard-wrap-default-column'.
- With plain `C-u', prompt for the column.
- With numeric prefix (e.g., `C-u 72'), use that as the column."
  (interactive
   (list
    :column
    (cond
     ((equal current-prefix-arg '(4))
      (read-number "Wrap at column: " (or (bound-and-true-p fill-column)
                                          night/hard-wrap-default-column)))
     ((numberp current-prefix-arg)
      (prefix-numeric-value current-prefix-arg))
     (t nil))
    :justify nil
    :filler nil))
  (let ((verbosity-level 0))
    (condition-case err
        (cl-labels
            ((night/h-region-bounds ()
               (cond
                ((use-region-p) (cons (region-beginning) (region-end)))
                (t (cons (point-min) (point-max))))))
          (let* ((col (cond
                       ((and (integerp column) (> column 0)) column)
                       ((and (integerp night/hard-wrap-default-column)
                             (> night/hard-wrap-default-column 0))
                        night/hard-wrap-default-column)
                       (t (or (bound-and-true-p fill-column) 80))))
                 (wrap-fn (cond
                           ((functionp filler) filler)
                           (t #'fill-region)))
                 (inhibit-read-only t)
                 (deactivate-mark nil)
                 (fill-column col))
            (run-hooks 'night/hard-wrap-before-hook)
            (save-excursion
              (save-restriction
                (widen)
                (pcase-let* ((`(,beg . ,end) (night/h-region-bounds)))
                  (funcall wrap-fn beg end justify))))
            (run-hooks 'night/hard-wrap-after-hook)
            (when (> verbosity-level 0)
              (message "Wrapped %s at column %d"
                       (cond
                        ((use-region-p) "region")
                        (t "buffer"))
                       col))))
      (error
       (user-error "night/hard-wrap-buffer: %s" (error-message-string err))))))

(provide 'night-hard-wrap)
;;; night-hard-wrap.el ends here

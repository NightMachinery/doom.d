;;; autoload/night-magit.el -*- lexical-binding: t; -*-

(after! (magit-mode)
  (map! :map magit-mode-map
        ;; Disable magit's =y= keybindings so that we can use the normal ones:
        :nvig "y" nil
        :nvig "Y" nil
        )
;;;
(defun night/git-patch-apply ()
  "Apply Git patch content from the clipboard to the current file."
  (interactive)
  (let ((patch-content (night/pbpaste))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer is not visiting a file"))
    ;; Save the buffer if modified
    (when (buffer-modified-p)
      (save-buffer))
    ;; Apply the patch using magit or git
    (let ((default-directory (file-name-directory filename)))
      (with-temp-buffer
        (insert patch-content)
        (let ((exit-code (call-process-region (point-min) (point-max)
                                              "git" nil "*Git Patch Output*" nil
                                              "apply" "-")))
          (if (eq exit-code 0)
              (progn
                (revert-buffer t t t)
                (message "Patch applied successfully"))
            (with-current-buffer "*Git Patch Output*"
              (error "Failed to apply patch:\n%s" (buffer-string)))))))))
;;;
(defcustom night/magit-diff-numeric-prefix "HEAD~"
  "The prefix to use when a numeric argument is given to `magit-diff`.
The number entered by the user will be appended to this string."
  :type 'string
  :group 'magit)

(defun night/h-magit-diff-range-advice (original-function &rest args)
  "Advise `magit-diff-range' to handle numeric input as `night/magit-diff-numeric-prefix'N.
When the user enters a number for the range, this function
intercepts it and formats it according to
`night/magit-diff-numeric-prefix' before passing it to the
original `magit-diff-range' function."
  (let ((verbosity-level 0)
        (range (car args)))
    (cond
     ((and (stringp range) (string-match-p "\\`[0-9]+\\'" range))
      (let ((new-range (concat night/magit-diff-numeric-prefix range)))
        (when (> verbosity-level 0)
          (message "Magit-diff: transformed numeric range '%s' to '%s'" range new-range))
        (apply original-function (cons new-range (cdr args)))))
     (t
      (apply original-function args)))))

(advice-add 'magit-diff-range :around #'night/h-magit-diff-range-advice)
;;;
  )

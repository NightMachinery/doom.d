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
  )

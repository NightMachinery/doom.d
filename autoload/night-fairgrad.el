;;; autoload/night-fairgrad.el -*- lexical-binding: t; -*-

(cl-defun night/h-is-fairgrad-tex-file ()
  "Check if current buffer is a .tex file in FairGrad project."
  (let ((verbosity-level 0))
    (when (>= verbosity-level 1)
      (message "Checking if file is FairGrad tex file"))
    (and (buffer-file-name)
         (string-equal (file-name-extension (buffer-file-name)) "tex")
         (let ((project-root (project-root (project-current))))
           (and project-root
                (string-equal (file-name-nondirectory (directory-file-name project-root))
                             "FairGrad"))))))

(cl-defun night/fairgrad-tex-save-hook ()
  "Hook to build FairGrad paper when saving .tex files in FairGrad project."
  (let ((verbosity-level 0))
    (condition-case err
        (progn
          (when (>= verbosity-level 1)
            (message "Running FairGrad tex save hook"))
          (when (night/h-is-fairgrad-tex-file)
            (when (>= verbosity-level 0)
              (message "Building FairGrad paper"))
            (comment
             ;; hook disabled
             (night/brishz 'awaysh
                           'fnswap 'h-fairgrad-paper-copy-files 'true
                           'fairgrad-paper-build))
            ))
      (error
       (message "Error in night/fairgrad-tex-save-hook: %s" (error-message-string err))))))

;; Add the hook to after-save-hook
;; (add-hook 'after-save-hook #'night/fairgrad-tex-save-hook)

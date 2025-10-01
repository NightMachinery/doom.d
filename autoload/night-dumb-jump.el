;;; autoload/night-dumb-jump.el -*- lexical-binding: t; -*-

(require 'dumb-jump)
(after! (dumb-jump)
  (setq dumb-jump-force-searcher 'rg)
;;;
  ;; @alt We can override `dumb-jump-get-language' to make .org files return `python` as their language, but the current solution is more robust, I think.
  (defun night/h-dumb-jump-add-python-rules-to-org ()
    "Add Python rules to Org mode in Dumb Jump."
    (interactive)
    (let ((python-rules (cl-remove-if-not (lambda (rule)
                                            (string= (plist-get rule :language) "python"))
                                          dumb-jump-find-rules)))
      (setq dumb-jump-find-rules
            (append dumb-jump-find-rules
                    (mapcar (lambda (rule)
                              (plist-put (copy-tree rule) :language "org"))
                            python-rules)))))
  (night/h-dumb-jump-add-python-rules-to-org)
;;;
  ;; Define the main function to jump to the LaTeX file at point
  (cl-defun night/latex-jump-to-file (&optional (verbosity-level 1))
    "Jump to the LaTeX file specified in \\input{...} or \\include{...} at point.
VERBOSITY-LEVEL controls messages: 0=silent, 1+=verbose. Defaults to 1.
Returns t on success, nil on failure."
    (interactive)
    (let* ((current-dir (night/h-get-project-root))
           (file-path (night/h-get-latex-file-path-at-point))
           (full-path (and file-path (expand-file-name file-path current-dir)))
           (full-path-tex (and file-path (concat full-path ".tex"))))
      (condition-case err
          (cond
           ((not file-path)
            (when (> verbosity-level 0)
              (message "No file path found at point."))
            nil)
           ((file-exists-p full-path)
            (find-file full-path)
            (when (> verbosity-level 0)
              (message "Opened file: %s" full-path))
            t)
           ((file-exists-p full-path-tex)
            (find-file full-path-tex)
            (when (> verbosity-level 0)
              (message "Opened file: %s" full-path-tex))
            t)
           (t
            (when (> verbosity-level 0)
              (message "File not found: %s" file-path))
            nil))
        (error
         (message "Error: %s" (error-message-string err))
         nil))))

  ;; Define an internal helper function to extract the file path at point
  (defun night/h-get-latex-file-path-at-point ()
    "Get the file path from \\input{...} or \\include{...} at point."
    (let ((regexp "\\\\\\(input.*?\\|include\\){\\([^}]+\\)}")
          result)
      (save-excursion
        (beginning-of-line)
        (cond
         ;; Search forward from the beginning of the line
         ((re-search-forward regexp (line-end-position) t)
          (setq result (match-string 2)))
         ;; If not found, search backward from point to the beginning of the line
         ((re-search-backward regexp (line-beginning-position) t)
          (setq result (match-string 2)))))
      result))

  ;; Define an internal helper function to get the current project root directory
  (defun night/h-get-project-root ()
    "Get the root directory of the current project."
    (or
     ;; Use 'projectile' if available
     (when (fboundp 'projectile-project-root)
       (projectile-project-root))
     ;; Use built-in 'project' package if available
     (when (fboundp 'project-current)
       (car (project-roots (project-current))))
     ;; Fallback to the directory of the current buffer's file
     (file-name-directory (buffer-file-name))))

  (comment
   ;; @o1 @broken
   (defun my-dumb-jump-latex-input-jump (symbol)
     "Open the LaTeX input file specified by SYMBOL."
     (let* ((dir (file-name-directory (buffer-file-name)))
            (file (expand-file-name symbol dir))
            (file-tex (concat file ".tex")))
       (cond
        ((file-exists-p file)
         (find-file file))
        ((file-exists-p file-tex)
         (find-file file-tex))
        (t (message "File not found: %s" symbol)))))
   (add-to-list 'dumb-jump-find-rules
                '(:type "include"
                  :language "tex"
                  :regex "\\\\input{\\([^}]+\\)}"
                  :supports ("tex")
                  :captures (1)
                  :jump-to my-dumb-jump-latex-input-jump)))

;;;
  )

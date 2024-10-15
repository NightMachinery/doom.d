;;; night-magit-ai.el ---                            -*- lexical-binding: t; -*-
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'magit)

(cl-defun night/detect-language (content &key (backend 'pygmentize))
  ;; @broken @o1
  ;; [[id:b6a25539-ddc3-4d3c-9c7a-7443a400bd24][Programming Language Detector]]
  ;; I cannot even get a PL detector to work on the CLI.
  ;;;
  "Detect the programming language of CONTENT using BACKEND and return the file extension."
  (let ((lang
         (pcase backend
           ('pygmentize
            ;; Use pygmentize for language detection
            (with-temp-buffer
              (insert content)
              ;; BUG should pass filename to pygmentize
              (shell-command-to-string "pygmentize -N -f token -l guess")))
           ('linguist
            ;; Use linguist for language detection
            (let ((temp-file (make-temp-file "linguist-")))
              (with-temp-file temp-file
                (insert content))
              (let ((output (shell-command-to-string
                             (format "linguist %s --json" temp-file))))
                (delete-file temp-file)
                (let* ((json-object-type 'hash-table)
                       (json-array-type 'list)
                       (json (json-read-from-string output))
                       (languages (gethash "languages" json)))
                  ;; Return the most probable language
                  (caar languages)))))
           (_ (error "Unsupported backend: %s" backend)))))
    ))

(cl-defun night/h-mai-generate-temp-path (prefix-path &key (mode 'simple-time))
  "Generate a directory path within PREFIX-PATH using the specified MODE.

MODE can be:
  - 'simple-time: 'HH:MM' format.
  - 'full-time: 'HH:MM:SS' format.
  - 'unix: Unix timestamp (seconds since epoch)."
  (let* ((formatted-time
          (pcase mode
            ('simple-time "%H:%M")
            ('full-time "%H:%M:%S")
            ('unix "%s")
            (_ (error "Unsupported mode: %s" mode))))
         (postfix (format-time-string formatted-time))
         (full-path (concat prefix-path postfix)))
    (cond
     ((file-exists-p full-path)
      ;; For 'simple-time' and 'full-time', append seconds or counter if necessary
      (when (eq mode 'simple-time)
        (setq postfix (format-time-string "%H:%M:%S")))

      ;; If directory still exists, append an incremental counter
      (let ((counter 1)
            (new-postfix postfix))
        (while (file-exists-p (concat prefix-path new-postfix))
          (setq new-postfix (format "%s_%d" postfix counter))
          (setq counter (1+ counter)))
        (concat prefix-path new-postfix)))
     (t
      ;; Directory does not exist, return the base path
      full-path))))

(cl-defun night/init-git-from-clipboard
    (&key
     ;; (ext 'py)
     ;; (ext 'auto)
     (ext 'auto-emacs)
     (git-backend 'magit))
  "Initialize a git repository from the clipboard content.
EXT specifies the file extension.
GIT-BACKEND can be 'magit or 'git."
  (interactive)
  (let* ((dir (night/h-mai-generate-temp-path
               (expand-file-name "~/tmp/mai/MAI_")))
         (content (night/pbpaste)))
    (unless content
      (error "Clipboard is empty"))
    ;; Create the directory
    (make-directory dir t)

    (cond
     ((eq ext 'auto)
      (setq ext (night/detect-language content :backend 'linguist))
      (setq ext ((or (cdr (assoc ext night/lang-ext-mapping))
                     ext))))

     ((eq ext 'auto-emacs)
      (setq ext night/last-yank-ext)))

    ;; Default if detection fails
    (unless ext
      (setq ext 'py))

    (setq ext (night/symbol-name ext))
    (let ((filename (concat dir "/main." ext)))
      ;; Write content to the file
      (with-temp-file filename
        (insert content))

      (when (equalp ext "py")
        (z reval-on-file-inplace (identity filename) whitespace-shared-rm))

      ;; Initialize git repository and commit
      (let ((default-directory dir))
        (pcase git-backend
          ('git
           ;; Use shell commands for Git operations
           (call-process "git" nil nil nil "init")
           (call-process "git" nil nil nil "add" (file-name-nondirectory filename))
           (call-process "git" nil nil nil "commit" "-m" "repo started"))
          ('magit
           ;; Use Magit functions for Git operations
           (magit-init dir)
           (magit-stage-file (list filename))
           (magit-commit-create '("-m" "repo started")))
          (_ (error "Unsupported git-backend: %s" git-backend))))

      ;; Open the file
      (find-file filename)

      ;; Open Magit status
      ;; (magit-status-setup-buffer dir)
      )))

;;;
(provide 'night-magit-ai)
;;; night-magit-ai.el ends here

;;; night-magit-ai.el ---                            -*- lexical-binding: t; -*-
;;; Code:
(require 'cl-lib)
(require 'json)
(require 'magit)


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

  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end))
    (setq deactivate-mark nil))

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
    (let ((filename (concat dir "/MAI." ext)))
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
           ;; Use Magit functions for Git operations without creating a magit status buffer
           (magit-call-git "init")
           (magit-call-git "add" (file-name-nondirectory filename))
           (magit-call-git "commit" "-m" "repo started"))
          ('magit-v1
           ;; Use Magit functions for Git operations
           (magit-init dir)
           (magit-stage-file (list filename))
           (magit-commit-create '("-m" "repo started")))
          (_ (error "Unsupported git-backend: %s" git-backend))))

      ;; Open the file
      (find-file filename)
      )))

(map!
 :leader
 "z a" #'night/MAI-init-git)
;;;
(provide 'night-magit-ai)
;;; night-magit-ai.el ends here

;;; ~/doom.d/night-buffer.el -*- lexical-binding: t; -*-
;;;
(after! (uniquify)
       (setq uniquify-buffer-name-style 'forward))
;;;
(defun night/force-kill-current-buffer ()
  (interactive)
  ;; [[https://emacs.stackexchange.com/questions/59348/force-kill-a-buffer][force kill a buffer? - Emacs Stack Exchange]]
  (let (kill-buffer-hook kill-buffer-query-functions)
    (kill-buffer)))

(defun night/force-kill-buffer (buffer)
  (with-current-buffer buffer
    (night/force-kill-current-buffer)))

(defun night/force-save-current-buffer ()
  (interactive)
  ;; Temporarily disable save-related hooks and functions
  (let (
        (write-file-functions nil)
        ;; `undo-fu-session--save-safe'
;;;
        (before-save-hook nil)
        (after-save-hook nil))
    (save-buffer)))
;;;
(defun night/buffer-reopen ()
  (interactive)
  (if-let (filename (or
                     buffer-file-name))
      (progn
        (kill-current-buffer)
        (let ((*night/h-interactive-buffer-force* t))
          (find-file-existing filename)))
    (error "Couldn't find filename in current buffer")))
;;;
(defun night/diff-buffers (buffer-A buffer-B)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive
   (list (read-buffer "buffer1: " (current-buffer))
         (read-buffer "buffer2: " (current-buffer))))
  ;; ediff might not be loaded, I don't know how it is loaded, but playing around with its commands does load it ...
  ;; (ediff-buffers-internal buffer-A buffer-B nil nil 'ediff-buffers)
  (ediff-buffers buffer-A buffer-B))

(defun night/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or
                     buffer-file-name
                     (and (bound-and-true-p list-buffers-directory)
                          list-buffers-directory)))
      (progn
        (kill-new filename)
        (message "Yanked: %s" filename))
    (error "Couldn't find filename in current buffer")))
(map! :leader
      "fy" #'night/yank-buffer-filename ; overrides doom's version
      )
;;;
(defun night/popup-buffer-p (buffer-or-name)
  "Return non-nil if BUFFER-OR-NAME is a popup buffer.
A popup buffer is defined as one whose name starts with a space
or is enclosed within asterisks (e.g., \"*Messages*\")."
  (let ((name (if (stringp buffer-or-name)
                  buffer-or-name
                (buffer-name buffer-or-name))))
    (and name
         (or (string-prefix-p " " name)
             (and
              (string-match-p "^\\s-*\\*" name)
              (not (string-match-p "^\\s-*\\*Org Src " name)))
             ;; (string-match-p "\\`\\*.*\\*\\'" name)
             ))))

(defun night/switch-to-last-buffer ()
  "Switch to the most recent non-popup buffer.
Skip buffers considered as popups until a suitable buffer is found."
  (interactive)
  (let ((buffers (buffer-list)))
    (catch 'found
      (dolist (buf buffers)
        (unless (or (eq buf (current-buffer))
                    (night/popup-buffer-p buf))
          (switch-to-buffer buf)
          (throw 'found t)))
      (message "No suitable buffer found."))))

(map!
 :nv
 "[[" #'night/switch-to-last-buffer)
(map! :map (evil-tex-mode-map)
 :nv
 "[[" #'night/switch-to-last-buffer)
;;;
(defun night/close-fileless-buffers ()
  "Close buffers that are not visiting a file, do not have an associated process,
whose names do not start with a *, and whose file paths no longer exist."
  (interactive)
  (dolist (buffer
           (buffer-list)

           ;; (list (current-buffer))
           ;; For debugging
           )
    (let ((file-name (buffer-file-name buffer))
          (buffer-name (buffer-name buffer)))

      ;; (message "file-name: %s\nexists: %s" file-name (file-exists-p file-name))
      (when
          (or
           (and
            file-name
            (s-ends-with-p ".gpg" file-name)
            ;; GPG buffers can cause emacs to ask for a password, so let's close them, too.
            )
           (and (or
                 (not file-name)
                 (not (file-exists-p file-name)))
                (not (get-buffer-process buffer))
                (not (night/popup-buffer-p buffer))))
        (message "Closing buffer: %s" buffer-name)
        (kill-buffer buffer)))))
(map! :leader
      "bo" #'night/close-fileless-buffers)
;;;

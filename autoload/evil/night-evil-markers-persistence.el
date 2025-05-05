;;; night-evil-markers-persistence.el --- Persistent Evil markers -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides persistence for Evil markers across Emacs sessions.
;; It handles both local (buffer-specific) and global markers.

;;; Code:
;; @todo Use JSON instead.
;;;
(after! (evil
         )
  (require 'cl-lib)
  (require 'debug)
  (require 'filenotify) ; Needed for backup file naming logic. Should be built-in.

  (defgroup night-evil-markers-persistence nil
    "Customization group for night's evil marker persistence."
    :group 'evil)

  (defcustom night-evil-auto-save-on-emacs-quit-markers-p nil
    "Whether to automatically save markers when quitting Emacs. Auto-saving the markers might not play well with multiple simultaneously open emacs sessions."
    :type 'boolean
    :group 'night-evil-markers-persistence)

  (defcustom night-evil-auto-save-on-buffer-quit-markers-p nil
    "Whether to automatically save markers when quitting a buffer. Auto-saving the markers might not play well with multiple simultaneously open emacs sessions."
    :type 'boolean
    :group 'night-evil-markers-persistence)

  (defcustom night-evil-load-all-markers-p nil
    "Whether to load all saved markers or just the requested one."
    :type 'boolean
    :group 'night-evil-markers-persistence)

  (defcustom night-evil-save-all-markers-p nil
    "Whether to save all markers or just the current one."
    :type 'boolean
    :group 'night-evil-markers-persistence)

  (defcustom night-evil-verbosity-level 1
    "Verbosity level for night-evil-markers-persistence operations.
0: Silent, 1: Basic information, 2: Detailed information, 3: Debug information"
    :type 'integer
    :group 'night-evil-markers-persistence)

  (defcustom night-evil-markers-persistence-extension ".evilmarkers"
    "File extension for evil marker files."
    :type 'string
    :group 'night-evil-markers-persistence)

  (defcustom night-evil-global-markers-file
    (expand-file-name (concat ".global" night-evil-markers-persistence-extension) user-emacs-directory)
    "File to store global evil markers."
    :type 'file
    :group 'night-evil-markers-persistence)

  (defun night/h-find-next-backup-filename (base-filename)
    "Find the next available numbered backup filename for BASE-FILENAME."
    (let ((n 1)
          backup-filename)
      (while (progn
               (setq backup-filename (format "%s.bak%d" base-filename n))
               (file-exists-p backup-filename))
        (setq n (1+ n)))
      backup-filename))

  (defun night/h-backup-file-with-log (file-name error-data)
  "Backup FILE-NAME to a numbered version and save ERROR-DATA traceback."
  (condition-case backup-err
      (when (file-exists-p file-name)
        (let* ((backup-file (night/h-find-next-backup-filename file-name))
               (log-file (concat backup-file ".log")))
          (rename-file file-name backup-file t) ; Use backup numbering feature

          ;; Save error message and backtrace
          (with-temp-buffer
            (insert (format "Error occurred while processing %s:\n\n" file-name))
            ;; Insert the specific error message captured by condition-case
            (insert (format "Error: %s\n\n" (error-message-string error-data)))
            ;; Insert the backtrace at the point of the error
            (insert "Backtrace:\n")
            (let ((backtrace-str (condition-case nil ; Avoid error if backtrace itself fails
                                     (backtrace-to-string (backtrace))
                                   (error "Failed to generate backtrace"))))
              (insert backtrace-str))
            ;; Write the collected info to the log file
            (write-region (point-min) (point-max) log-file nil 'silent))

          (message "Error processing marker file %s. Backed up to %s. See %s for details."
                   file-name backup-file log-file)
          backup-file))
    (error
     ;; Error during backup process itself
     (message "Critical error: Failed to backup corrupted marker file %s. Error: %s"
              file-name (error-message-string backup-err))
     nil)))

  (defun night/evil-marker-file-name (buffer-file-name)
    "Generate the marker file name for the given BUFFER-FILE-NAME."
    (cond
     ((not buffer-file-name)
      (when (> night-evil-verbosity-level 2)
        (message "Cannot generate marker file name: buffer is not associated with a file"))
      nil)
     ((or
       (string-suffix-p night-evil-markers-persistence-extension buffer-file-name)
       (string-suffix-p ".gpg" buffer-file-name))
      nil)
     (t
      (let* ((dir (file-name-directory buffer-file-name))
             (file (file-name-nondirectory buffer-file-name))
             (prefix (if (string-prefix-p "." file) "" ".")))
        (concat dir prefix file night-evil-markers-persistence-extension)))))

  (defun night/serialize-marker (marker)
    "Serialize MARKER to a storable format."
    (let* ((deserialized-marker
            (cond
             ((markerp marker)
              (when-let* ((pos (marker-position marker))
                          (buf (marker-buffer marker))
                          (file (buffer-file-name buf)))
                (cons file pos)))
             ((functionp marker) nil) ; Functions cannot be serialized
             ((consp marker)
              (when-let* ((file (car marker))
                          (pos (cadr marker)))
                (when (and file (stringp file) (file-exists-p file) pos (integerp pos))
                  (cons file pos))))
             (t nil))))
      deserialized-marker))

  (defun night/deserialize-marker (serialized)
    "Deserialize the SERIALIZED marker."
    (when (consp serialized)
      (let ((file (car serialized))
            (pos (cdr serialized)))
        (when (and file (stringp file) pos (integerp pos))
          (or (when-let ((buf (find-buffer-visiting file)))
                (set-marker (make-marker) pos buf))
              serialized))))) ; Return serialized form if buffer not found

  (defun night/process-marker-alist (alist process-fn)
    "Process ALIST with PROCESS-FN and remove nil results and excluded keys."
    (let* ((excluded-keys '(91 93 94))  ; Keys to exclude: [, ], ^
           (res
            (cl-remove-if-not
             (lambda (entry)
               (and (consp entry) ; Ensure it's a pair
                    (cdr entry)   ; Ensure value is not nil after processing attempt
                    (not (memq (car entry) excluded-keys))))
             (mapcar (lambda (entry)
                       (when (consp entry) ; Process only valid pairs
                         (cons (car entry)
                               (funcall process-fn (cdr entry)))))
                     alist)))
           (res (cl-remove-duplicates res :key #'car :from-end t)))
      res))

  (defun night/handle-markers-file (file-name operation &optional data)
    "Handle marker file operations for FILE-NAME.
OPERATION is 'read, 'write, or 'read-single. DATA is used for write operations.
On error, attempts to backup the problematic file and returns nil."
    (unless (member operation '(read write read-single))
      (error "Invalid operation: %s" operation))
    (when file-name                     ; Only proceed if file-name is non-nil
      (condition-case err
          (pcase operation
            ('read
             (when (file-exists-p file-name)
               (with-temp-buffer
                 (insert-file-contents file-name)
                 (goto-char (point-min))
                 (night/process-marker-alist (read (current-buffer)) #'night/deserialize-marker))))
            ('write
             (unless data (error "No data provided for write operation"))
             (make-directory (file-name-directory file-name) t) ; Ensure directory exists
             (let* ((existing-markers (night/handle-markers-file file-name 'read)) ; Read might fail, handled by outer condition-case
                    (markers-to-save (night/process-marker-alist
                                      (if (consp (car data)) ; Check if data is a list of markers or a single one
                                          (append data existing-markers)
                                        (cons data existing-markers))
                                      #'night/serialize-marker)))
               (if (null markers-to-save)
                   (progn
                     (when (file-exists-p file-name)
                       (delete-file file-name))
                     (when (> night-evil-verbosity-level 1)
                       (message "No markers to save. Deleted %s" file-name)))
                 (with-temp-file file-name
                   (let ((print-level nil)
                         (print-length nil))
                     (prin1 markers-to-save (current-buffer)))))))
            ('read-single
             (unless data (error "No marker character provided for read-single operation"))
             (when (file-exists-p file-name)
               (with-temp-buffer
                 (insert-file-contents file-name)
                 (goto-char (point-min))
                 (when-let* ((markers (read (current-buffer))) ; read can signal an error
                             (marker-entry (assoc data markers)))
                   (when marker-entry ; Ensure entry exists
                     (let ((deserialized (night/deserialize-marker (cdr marker-entry))))
                       (when deserialized ; Ensure deserialization worked
                         (cons (car marker-entry) deserialized)))))))))
        ;; Error handler for the main operations
        (error
         ;; Backup the file and log the error
         (night/h-backup-file-with-log file-name err)
         ;; Log the error for debugging if verbosity allows (backup message already shown)
         (when (> night-evil-verbosity-level 0)
             (message "Error %s markers %s: %s (details in backup log)"
                      (pcase operation ('read "reading") ('write "writing") ('read-single "reading"))
                      (if (eq operation 'read-single) "entry" "file")
                      (error-message-string err)))
         nil)))) ; Return nil on error

  (cl-defun night/save-markers (&key (global-p nil global-p-supplied-p) char)
    "Save evil markers. If GLOBAL-P, save global markers. If CHAR, save only that marker."
    (let ((verbosity-level night-evil-verbosity-level)) ; Use custom variable
      (when (and char global-p-supplied-p)
        (cl-assert (eq global-p (evil-global-marker-p char)) nil
                   "Supplied global-p must match (evil-global-marker-p char) when char is provided"))
      (let* ((is-global (if char (evil-global-marker-p char) global-p))
             (current-buffer-file-name buffer-file-name) ; Capture buffer-file-name in case buffer changes
             (file-name (if is-global
                            night-evil-global-markers-file
                          (night/evil-marker-file-name current-buffer-file-name)))
             (markers-alist (if is-global
                                (default-value 'evil-markers-alist)
                              evil-markers-alist))
             (markers-to-save
              (cond
               ((and char (not night-evil-save-all-markers-p))
                (when-let ((marker (assoc char markers-alist)))
                  (list marker)))
               (t
                markers-alist))))
        (when file-name
          (when (> verbosity-level 1)
            (message "Attempting to save markers%s to %s..."
                     (if char (format " for char '%c'" char) "")
                     file-name))
          (night/handle-markers-file file-name 'write markers-to-save)))))

  (cl-defun night/load-markers (&key global-p char (global-p-supplied-p nil))
    "Load evil markers. If GLOBAL-P, load global markers. If CHAR, load only that marker."
    (let ((verbosity-level night-evil-verbosity-level)) ; Use custom variable
      (when (and char global-p-supplied-p)
        (cl-assert (eq global-p (evil-global-marker-p char)) nil
                   "Supplied global-p must match (evil-global-marker-p char) when char is provided"))
      (let* ((is-global (if char (evil-global-marker-p char) global-p))
             (current-buffer-file-name buffer-file-name) ; Capture buffer-file-name
             (file-name (if is-global
                            night-evil-global-markers-file
                          (night/evil-marker-file-name current-buffer-file-name)))
             new-markers)
        (when file-name
          (when (> verbosity-level 1)
            (message "Attempting to load markers%s from %s..."
                     (if char (format " for char '%c'" char) "")
                     file-name))
          (if (and char (not night-evil-load-all-markers-p))
              (when-let ((marker-entry (night/handle-markers-file file-name 'read-single char)))
                (when marker-entry
                   (let* ((current-alist (if is-global
                                            (default-value 'evil-markers-alist)
                                          evil-markers-alist))
                         (cleaned-alist (assq-delete-all (car marker-entry) current-alist)))
                    (setq new-markers (cons marker-entry cleaned-alist)))))
            (when-let ((loaded-markers (night/handle-markers-file file-name 'read)))
              (let* ((current-alist (if is-global
                                       (default-value 'evil-markers-alist)
                                     evil-markers-alist))
                     (filtered-current (cl-remove-if (lambda (entry) (assq (car entry) loaded-markers))
                                                     current-alist)))
                (setq new-markers (append loaded-markers filtered-current)))))

          (when new-markers
            (if is-global
                (setq-default evil-markers-alist new-markers)
              (setq-local evil-markers-alist new-markers))
            (when (> verbosity-level 1)
              (message "Successfully loaded markers%s from %s."
                       (if char (format " for char '%c'" char) "") file-name)))
          (unless new-markers
            (when (> verbosity-level 1)
              (message "No markers loaded%s from %s (file might not exist, be empty, or contain only invalid entries)."
                       (if char (format " for char '%c'" char) "") file-name)))))))


  (defun night/evil-set-marker (orig-fun char &optional pos advance)
    "Advice to save markers to a file after setting them."
    (let ((result (funcall orig-fun char pos advance)))
      ;; Ensure the marker was actually set before saving
      (when result
        (night/save-markers :char char))
      result))

  (defun night/evil-get-marker (orig-fun char &optional raw)
    "Advice to load markers from a file if they're not in evil-markers-alist."
    (let ((marker (funcall orig-fun char raw)))
      (cond (marker marker) ; Marker found, return it
            ((or buffer-file-name (evil-global-marker-p char)) ; File exists or global marker
             (night/load-markers :char char) ; Attempt to load the specific marker
             (funcall orig-fun char raw)) ; Try getting it again
            (t nil)))) ; No file and not global, cannot load

  (advice-add 'evil-set-marker :around #'night/evil-set-marker)
  (advice-add 'evil-get-marker :around #'night/evil-get-marker)

  (defun night/save-evil-markers-on-emacs-quit ()
    "Save evil markers when quitting Emacs."
    (let ((verbosity-level night-evil-verbosity-level))
      (when night-evil-auto-save-on-emacs-quit-markers-p
        (when (> verbosity-level 0) (message "Auto-saving Evil markers on Emacs quit..."))
        (dolist (buffer (buffer-list))
          (with-current-buffer buffer
            (when buffer-file-name
              (night/save-markers :global-p nil))))
        (night/save-markers :global-p t)
        (when (> verbosity-level 0) (message "Auto-saving Evil markers on Emacs quit... Done.")))))

  (defun night/save-evil-markers-on-buffer-quit ()
    "Save evil markers when quitting a buffer."
    (let ((verbosity-level night-evil-verbosity-level))
      (when (and night-evil-auto-save-on-buffer-quit-markers-p buffer-file-name)
        (when (> verbosity-level 1)
          (message "Auto-saving Evil markers for buffer %s on quit..." (buffer-name)))
        (night/save-markers :global-p nil))))

  (add-hook 'kill-emacs-hook #'night/save-evil-markers-on-emacs-quit)
  (add-hook 'kill-buffer-hook #'night/save-evil-markers-on-buffer-quit)

  (defun night/manually-save-evil-markers ()
    "Manually save evil markers for the current buffer and global markers."
    (interactive)
    (let ((verbosity-level night-evil-verbosity-level))
      (when buffer-file-name
        (night/save-markers :global-p nil)
        (when (> verbosity-level 0)
          (message "Evil markers saved for the current buffer.")))
      (night/save-markers :global-p t)
      (when (> verbosity-level 0)
        (message "Global evil markers saved."))))

  (defun night/manually-load-evil-markers ()
    "Manually load evil markers for the current buffer and global markers."
    (interactive)
    (let ((verbosity-level night-evil-verbosity-level))
      (when buffer-file-name
        (night/load-markers :global-p nil)
        (when (> verbosity-level 0)
          (message "Evil markers loaded for the current buffer.")))
      (night/load-markers :global-p t)
      (when (> verbosity-level 0)
        (message "Global evil markers loaded."))))

  (provide 'night-evil-markers-persistence)

;;; night-evil-markers-persistence.el ends here
  )

;;; night-evil-markers.el --- Persistent Evil markers -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides persistence for Evil markers across Emacs sessions.
;; It handles both local (buffer-specific) and global markers.

;;; Code:

(after! (evil
         )
  (defgroup night-evil-markers nil
    "Customization group for night's evil marker persistence."
    :group 'evil)

  (defcustom night-evil-auto-save-markers-p nil
    "Whether to automatically save markers when quitting Emacs."
    :type 'boolean
    :group 'night-evil-markers)

  (defcustom night-evil-load-all-markers-p nil
    "Whether to load all saved markers or just the requested one."
    :type 'boolean
    :group 'night-evil-markers)

  (defcustom night-evil-save-all-markers-p nil
    "Whether to save all markers or just the current one."
    :type 'boolean
    :group 'night-evil-markers)

  (defcustom night-evil-verbosity-level 1
    "Verbosity level for night-evil-markers operations.
0: Silent, 1: Basic information, 2: Detailed information, 3: Debug information"
    :type 'integer
    :group 'night-evil-markers)

  (defcustom night-evil-global-markers-file
    (expand-file-name ".global-evil-markers" user-emacs-directory)
    "File to store global evil markers."
    :type 'file
    :group 'night-evil-markers)

  (defun night/evil-marker-file-name (buffer-file-name)
    "Generate the marker file name for the given BUFFER-FILE-NAME."
    (cond
     ((not buffer-file-name)
      (when (> night-evil-verbosity-level 0)
        (message "Cannot generate marker file name: buffer is not associated with a file"))
      nil)
     ((string-suffix-p ".evilmarkers" buffer-file-name)
      nil)
     (t
      (let* ((dir (file-name-directory buffer-file-name))
             (file (file-name-nondirectory buffer-file-name))
             (prefix (if (string-prefix-p "." file) "" ".")))
        (concat dir prefix file ".evilmarkers")))))

  (defun night/serialize-marker (marker)
    "Serialize MARKER to a storable format."
    (let* ((deserialized-marker
            (cond
             ((markerp marker)
              (when-let* ((pos (marker-position marker))
                          (buf (marker-buffer marker))
                          (file (buffer-file-name buf)))
                (cons file pos)))
             ((functionp marker) nil)
             ((consp marker) marker)
             (t nil))))
      deserialized-marker))

  (defun night/deserialize-marker (serialized)
    "Deserialize the SERIALIZED marker."
    (when (consp serialized)
      (let ((file (car serialized))
            (pos (cdr serialized)))
        (or (when-let ((buf (find-buffer-visiting file)))
              (set-marker (make-marker) pos buf))
            serialized))))

  (defun night/process-marker-alist (alist process-fn)
    "Process ALIST with PROCESS-FN and remove nil results."
    (cl-remove-if-not #'cdr
                      (mapcar (lambda (entry)
                                (cons (car entry)
                                      (funcall process-fn (cdr entry))))
                              alist)))

  (defun night/handle-markers-file (file-name operation &optional data)
    "Handle marker file operations for FILE-NAME.
OPERATION is 'read, 'write, or 'read-single. DATA is used for write operations."
    (unless (member operation '(read write read-single))
      (error "Invalid operation: %s" operation))
    (when file-name  ; Only proceed if file-name is non-nil
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
             (make-directory (file-name-directory file-name) t)
             (let* ((existing-markers (night/handle-markers-file file-name 'read))
                    (markers-to-save (night/process-marker-alist
                                      (if (consp (car data))
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
                 (when-let* ((markers (read (current-buffer)))
                             (marker-entry (assoc data markers)))
                   (cons (car marker-entry)
                         (night/deserialize-marker (cdr marker-entry))))))))
        (error
         (debug nil err)
         (when (> night-evil-verbosity-level 0)
           (message "Error %s markers %s: %s"
                    (pcase operation ('read "reading") ('write "writing") ('read-single "reading"))
                    (if (eq operation 'read-single) "entry" "file")
                    (error-message-string err)))
         nil))))

  (cl-defun night/save-markers (&key (global-p nil global-p-supplied-p) char)
    "Save evil markers. If GLOBAL-P, save global markers. If CHAR, save only that marker."
    (when (and char global-p-supplied-p)
      (cl-assert (eq global-p (evil-global-marker-p char)) nil
                 "Supplied global-p must match (evil-global-marker-p char) when char is provided"))
    (let* ((global-p (if char (evil-global-marker-p char) global-p))
           (file-name (if global-p
                          night-evil-global-markers-file
                        (night/evil-marker-file-name buffer-file-name)))
           (markers-alist (if global-p
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
        (night/handle-markers-file file-name 'write markers-to-save))))

  (cl-defun night/load-markers (&key global-p char (global-p-supplied-p nil))
    "Load evil markers. If GLOBAL-P, load global markers. If CHAR, load only that marker."
    (when (and char global-p-supplied-p)
      (cl-assert (eq global-p (evil-global-marker-p char)) nil
                 "Supplied global-p must match (evil-global-marker-p char) when char is provided"))
    (let* ((global-p (if char (evil-global-marker-p char) global-p))
           (file-name (if global-p
                          night-evil-global-markers-file
                        (night/evil-marker-file-name buffer-file-name)))
           new-markers)
      (when file-name
        (if (and char (not night-evil-load-all-markers-p))
            (when-let ((marker (night/handle-markers-file file-name 'read-single char)))
              (setq new-markers
                    (cons marker (assq-delete-all (car marker)
                                                  (if global-p
                                                      (default-value 'evil-markers-alist)
                                                    evil-markers-alist)))))
          (when-let ((markers (night/handle-markers-file file-name 'read)))
            (setq new-markers
                  (append markers
                          (cl-remove-if (lambda (entry) (assq (car entry) markers))
                                        (if global-p
                                            (default-value 'evil-markers-alist)
                                          evil-markers-alist))))))
        (when new-markers
          (if global-p
              (setq-default evil-markers-alist new-markers)
            (setq-local evil-markers-alist new-markers))))))

  (defun night/evil-set-marker (orig-fun char &optional pos advance)
    "Advice to save markers to a file after setting them."
    (let ((result (funcall orig-fun char pos advance)))
      (night/save-markers :char char)
      result))

  (defun night/evil-get-marker (orig-fun char &optional raw)
    "Advice to load markers from a file if they're not in evil-markers-alist."
    (or (funcall orig-fun char raw)
        (when (or buffer-file-name (evil-global-marker-p char))
          (night/load-markers :char char)
          (funcall orig-fun char raw))))

  (advice-add 'evil-set-marker :around #'night/evil-set-marker)
  (advice-add 'evil-get-marker :around #'night/evil-get-marker)

  (defun night/save-evil-markers-on-quit ()
    "Save evil markers when quitting Emacs."
    (when night-evil-auto-save-markers-p
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when buffer-file-name
            (night/save-markers :global-p nil))))
      (night/save-markers :global-p t)))

  (add-hook 'kill-emacs-hook #'night/save-evil-markers-on-quit)

  (defun night/manually-save-evil-markers ()
    "Manually save evil markers for the current buffer and global markers."
    (interactive)
    (when buffer-file-name
      (night/save-markers :global-p nil)
      (when (> night-evil-verbosity-level 0)
        (message "Evil markers saved for the current buffer.")))
    (night/save-markers :global-p t)
    (when (> night-evil-verbosity-level 0)
      (message "Global evil markers saved.")))

  (defun night/manually-load-evil-markers ()
    "Manually load evil markers for the current buffer and global markers."
    (interactive)
    (when buffer-file-name
      (night/load-markers :global-p nil)
      (when (> night-evil-verbosity-level 0)
        (message "Evil markers loaded for the current buffer.")))
    (night/load-markers :global-p t)
    (when (> night-evil-verbosity-level 0)
      (message "Global evil markers loaded.")))

  (provide 'night-evil-markers)

;;; night-evil-markers.el ends here
  )

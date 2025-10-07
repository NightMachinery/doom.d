;;;
(defvar night/current-theme-light nil
  "Theme to use in light mode.")

(defvar night/current-theme-dark nil
  "Theme to use in dark mode.")

(defvar night/h-dark-mode-active nil
  "Internal variable tracking whether dark mode is currently active.")

(defcustom night/dark-mode-poll-interval 15
  "Interval in seconds to poll system appearance."
  :type 'number
  :group 'night)

(defcustom night/dark-mode-follow-system-mode
  ;; nil
  'poll
  "How to synchronize with system appearance.
nil - Don't follow system appearance
'poll - Poll system appearance periodically"
  :type '(choice (const :tag "Don't follow system" nil)
          (const :tag "Poll system appearance" poll))
  :group 'night)

(defvar night/h-system-appearance-timer nil
  "Timer for polling system appearance.")

(defun night/h-os-dark-mode-p ()
  "Check if OS is in dark mode."
  (condition-case nil
      (zb dark-mode-p)
    (error nil)))

(defun night/dark-mode-p ()
  "Return non-nil if dark mode is currently active."
  night/h-dark-mode-active)

(defun night/enable-current-theme ()
  "Enable the current theme based on dark mode state."
  (interactive)
  (night/h-apply-theme night/h-dark-mode-active))

(defun night/h-apply-theme (dark-mode-p)
  "Apply the appropriate theme based on DARK-MODE-P."
  (let ((theme (cond
                (dark-mode-p night/current-theme-dark)
                (t night/current-theme-light))))
    (cond
     (theme
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t)
      (setq doom-theme theme)
      (setq night/h-dark-mode-active dark-mode-p)
      (message "Theme set for %s mode" (if dark-mode-p "dark" "light")))
     (t
      (message "Warning: Theme not set for %s mode" (if dark-mode-p "dark" "light"))))))

(defun night/theme-dark-mode-toggle ()
  "Toggle between light and dark themes."
  (interactive)
  (setq night/dark-mode-follow-system-mode nil)
  (night/h-setup-system-appearance-sync)
  (night/h-apply-theme (not night/h-dark-mode-active)))

(defun night/h-sync-with-system-appearance ()
  "Synchronize Emacs theme with system appearance."
  (let ((system-dark-mode (night/h-os-dark-mode-p)))
    (when (not (eq system-dark-mode night/h-dark-mode-active))
      (night/h-apply-theme system-dark-mode))))

(defun night/h-start-system-appearance-polling ()
  "Start polling system appearance."
  (night/h-stop-system-appearance-polling)
  (setq night/h-system-appearance-timer
        (run-with-timer 0 night/dark-mode-poll-interval #'night/h-sync-with-system-appearance)))

(defun night/h-stop-system-appearance-polling ()
  "Stop polling system appearance."
  (when night/h-system-appearance-timer
    (cancel-timer night/h-system-appearance-timer)
    (setq night/h-system-appearance-timer nil)))

(defun night/h-setup-system-appearance-sync ()
  "Setup system appearance synchronization based on `night/dark-mode-follow-system-mode'."
  (when (and night/current-theme-light night/current-theme-dark)
    (cond
     ((eq night/dark-mode-follow-system-mode 'poll)
      (night/h-stop-system-appearance-polling)
      (night/h-start-system-appearance-polling))
     (t
      (night/h-stop-system-appearance-polling)
      (night/enable-current-theme)))))

(defun night/dark-mode-follow-system-mode-set (value)
  "Set `night/dark-mode-follow-system-mode' to VALUE and update sync behavior."
  (interactive
   (list (intern (completing-read "Follow system mode: "
                                  '("nil" "poll")
                                  nil t))))
  (setq night/dark-mode-follow-system-mode value)
  (night/h-setup-system-appearance-sync))

;;;
(provide 'night-dark-mode)

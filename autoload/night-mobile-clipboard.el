;;; night-mobile-clipboard.el -*- lexical-binding: t; -*-

(require 'term/xterm)
(require 'select)
(load (expand-file-name "night-mobile-clipboard-ssh.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defcustom night/mobile-clipboard-max-bytes 6000
  "Maximum UTF-8 bytes copied from a mobile frame through OSC 52.
The conservative default fits Termux's 8192-character OSC buffer after
base64 encoding.  Larger copies stay in the kill ring, with a message.
Increase this only when the terminal and every multiplexer support it."
  :type 'natnum
  :group 'xterm)

(defvar night/h-mobile-clipboard-import-p nil
  "Non-nil while importing an SSH clipboard selection into the kill ring.")

(defvar night/h-mobile-clipboard-paste-inhibit-p nil
  "Non-nil when a paste command must only rotate the existing kill ring.")

(defcustom night/ssh-paste-enabled-p t
  "Whether ordinary paste reads opted-in mobile frames through SSH."
  :type 'boolean
  :group 'xterm)

(defun night/ssh-paste-toggle (&optional enable)
  "Toggle automatic SSH paste, or ENABLE it with a positive prefix argument."
  (interactive "P")
  (setq night/ssh-paste-enabled-p
        (cond (enable (> (prefix-numeric-value enable) 0))
              (t (not night/ssh-paste-enabled-p))))
  (when (called-interactively-p 'interactive)
    (message "Automatic SSH paste %s"
             (cond (night/ssh-paste-enabled-p "enabled") (t "disabled"))))
  night/ssh-paste-enabled-p)

(defun night/h-mobile-clipboard-insert (text)
  "Insert clipboard TEXT using the configured yank insertion helper."
  (cond ((fboundp 'night/insert-for-yank) (night/insert-for-yank text))
        (t (insert-for-yank text))))

(defun night/ssh-paste ()
  "Paste through the selected frame's explicitly configured SSH host."
  (interactive "*")
  (let ((host (frame-parameter nil 'night/clipboard-ssh-host))
        (command (frame-parameter nil 'night/clipboard-ssh-paste-command)))
    (unless (night/h-mobile-clipboard-ssh-valid-host-p host)
      (user-error "No valid SSH clipboard host configured for this frame"))
    (night/h-mobile-clipboard-insert
     (night/h-mobile-clipboard-ssh-get host command))))

(defun night/tealy-paste ()
  "Paste tealy's Termux clipboard, regardless of frame and toggle state."
  (interactive "*")
  (night/h-mobile-clipboard-insert
   (night/h-mobile-clipboard-ssh-get "tealy" "termux-clipboard-get")))

(defun night/mobile-clipboard-frame-p ()
  "Return non-nil for a terminal frame marked by `emc-mobile'."
  (and (not (display-graphic-p))
       (frame-parameter nil 'night/mobile)))

(defun night/h-mobile-clipboard-current-kill (orig-fun &rest args)
  "Route native clipboard reads through SSH in an opted-in mobile frame."
  (let ((host (frame-parameter nil 'night/clipboard-ssh-host)))
    (cond
     ((or night/h-mobile-clipboard-paste-inhibit-p
          (not (equal (car args) 0)))
      (apply orig-fun args))
     ((and night/ssh-paste-enabled-p
           (night/mobile-clipboard-frame-p)
           host
           (not (night/h-mobile-clipboard-ssh-valid-host-p host)))
      (user-error "Invalid mobile clipboard SSH host"))
     ((and night/ssh-paste-enabled-p
           (not night/h-mobile-clipboard-paste-inhibit-p)
           (night/mobile-clipboard-frame-p)
           (night/h-mobile-clipboard-ssh-valid-host-p host))
      (let* ((command (frame-parameter nil 'night/clipboard-ssh-paste-command))
            (interprogram-paste-function
             (lambda ()
               (night/h-mobile-clipboard-ssh-get host command)))
            (night/h-mobile-clipboard-import-p t)
            (kill-do-not-save-duplicates t))
        (apply orig-fun args)))
     (t (apply orig-fun args)))))

(advice-add 'current-kill :around #'night/h-mobile-clipboard-current-kill)

(defun night/h-mobile-clipboard-evil-paste-pop (orig-fun &rest args)
  "Prevent Evil's inner zero-index lookup from fetching a fresh clipboard."
  (let ((host (frame-parameter nil 'night/clipboard-ssh-host)))
    (cond
     ((and night/ssh-paste-enabled-p
           (night/mobile-clipboard-frame-p)
           (night/h-mobile-clipboard-ssh-valid-host-p host))
      (let ((night/h-mobile-clipboard-paste-inhibit-p t)
            (interprogram-paste-function nil))
        (apply orig-fun args)))
     (t (apply orig-fun args)))))

(defun night/h-mobile-clipboard-xterm-paste (orig-fun &rest args)
  "Preserve the bracketed-paste text provider while suppressing SSH reads."
  (let ((night/h-mobile-clipboard-paste-inhibit-p t))
    (apply orig-fun args)))

(with-eval-after-load 'evil
  (advice-add 'evil-paste-pop :around
              #'night/h-mobile-clipboard-evil-paste-pop))

(with-eval-after-load 'term/xterm
  ;; Bracketed paste supplies its own text provider around `yank'.
  (advice-add 'xterm-paste :around
              #'night/h-mobile-clipboard-xterm-paste))

(defun night/mobile-clipboard-cut (text)
  "Copy TEXT through OSC 52, with opt-in SSH fallback for oversized text.
The frame parameter `night/clipboard-ssh-host' selects a host alias from the
daemon host's SSH configuration.  Unmarked mobile frames remain OSC 52 only."
  (when (night/mobile-clipboard-frame-p)
    (let ((host (frame-parameter nil 'night/clipboard-ssh-host)))
      (cond
       ((night/h-mobile-clipboard-ssh-valid-host-p host)
        (night/h-mobile-clipboard-ssh-enqueue :host host :text text))
       (t (night/h-mobile-clipboard-osc52-cut text))))))

(defun night/h-mobile-clipboard-osc52-cut (text)
  "Copy TEXT through the selected mobile terminal using native OSC 52.
Do not forward to the daemon host's clipboard.  OSC 52 has no write
acknowledgement: a successful send cannot prove the terminal accepted it."
  (when (night/mobile-clipboard-frame-p)
    (let* ((bytes (string-bytes (encode-coding-string text 'utf-8-unix)))
           (terminal (frame-terminal))
           (previous (terminal-parameter terminal 'xterm--set-selection)))
      (cond
       ((> bytes night/mobile-clipboard-max-bytes)
        (message "Mobile clipboard: %d UTF-8 bytes exceeds limit %d; text remains in kill ring"
                 bytes night/mobile-clipboard-max-bytes))
       (t
        ;; Activate the native backend only for this operation.  Terminal
        ;; parameters can be shared by several frames; don't change policy
        ;; for other frames or enable clipboard reads.
        (unwind-protect
            (condition-case err
                (let ((xterm-max-cut-length
                       (* 4 (/ (+ night/mobile-clipboard-max-bytes 2) 3))))
                  (set-terminal-parameter terminal 'xterm--set-selection t)
                  (gui-set-selection 'CLIPBOARD text))
              (error
               (message "Mobile clipboard send failed: %s; text remains in kill ring"
                        (error-message-string err))))
          (set-terminal-parameter terminal 'xterm--set-selection previous)))))))

(provide 'night-mobile-clipboard)

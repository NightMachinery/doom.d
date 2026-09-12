;;; night-mobile-clipboard.el -*- lexical-binding: t; -*-

(require 'term/xterm)
(require 'select)

(defcustom night/mobile-clipboard-max-bytes 6000
  "Maximum UTF-8 bytes copied from a mobile frame through OSC 52.
The conservative default fits Termux's 8192-character OSC buffer after
base64 encoding.  Larger copies stay in the kill ring, with a message.
Increase this only when the terminal and every multiplexer support it."
  :type 'natnum
  :group 'xterm)

(defun night/mobile-clipboard-frame-p ()
  "Return non-nil for a terminal frame marked by `emc-mobile'."
  (and (not (display-graphic-p))
       (frame-parameter nil 'night/mobile)))

(defun night/mobile-clipboard-cut (text)
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

;;; -*- lexical-binding: t; -*-
(setq native-comp-enable-subr-trampolines nil)
(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar mobile-clipboard-test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(load (expand-file-name "autoload/night-mobile-clipboard.el" mobile-clipboard-test-root)
      nil t)

;; Load the actual copy advice without the rest of the interactive Doom setup.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "autoload/night-clipboard.el" mobile-clipboard-test-root))
  (goto-char (point-min))
  (let (form)
    (while (< (point) (point-max))
      (setq form (read (current-buffer)))
      (when (and (eq (car-safe form) 'defun)
                 (eq (cadr form) 'night/h-kill-skip-whitespace))
        (eval form t)
        (goto-char (point-max))))))

(defvar night/advice-kill-new-unescape-org-enabled-p nil)
(defalias 'equalp #'cl-equalp)

(defmacro mobile-clipboard-test-with-routing (&rest body)
  `(let* ((events nil)
         (kill-ring nil)
         (kill-ring-yank-pointer nil)
         (save-interprogram-paste-before-kill nil)
         (interprogram-cut-function (lambda (_text) (push 'host events)))
         (mobile t))
     (cl-letf (((symbol-function 'night/mobile-clipboard-frame-p) (lambda () mobile))
               ((symbol-function 'night/ssh-p) (lambda () t))
               ((symbol-function 'night/call-process-async)
                (lambda (&rest _args) (push 'legacy events)))
               ((symbol-function 'send-string-to-terminal)
                (lambda (text &optional _terminal) (push text events))))
       (unwind-protect
           (progn
             (advice-add 'kill-new :around #'night/h-kill-skip-whitespace)
             ,@body)
         (advice-remove 'kill-new #'night/h-kill-skip-whitespace)))))

(ert-deftest mobile-clipboard-native-utf8-and-frame-isolation ()
  (mobile-clipboard-test-with-routing
   (let ((text "hello فارسی 🦊\n\n")
         (prior (terminal-parameter nil 'xterm--set-selection)))
     (kill-new text)
     (should (equal events
                    (list (concat "\e]52;c;"
                                  (base64-encode-string
                                   (encode-coding-string text 'utf-8-unix) t)
                                  "\a"))))
     (should (equal (car kill-ring) text))
     (should (eq (terminal-parameter nil 'xterm--set-selection) prior)))
   ;; The same buffer next copied from a desktop frame keeps the host route.
   (setq mobile nil events nil)
   (kill-new "desktop")
   (should (equal events '(legacy host)))))

(ert-deftest mobile-clipboard-limit-and-append ()
  (mobile-clipboard-test-with-routing
   (let ((night/mobile-clipboard-max-bytes 6))
     (kill-new "ééé")
     (should (= (length events) 1))
     (setq events nil)
     (kill-append "x" nil)
     (should (equal (car kill-ring) "éééx"))
     (should-not events))))

(ert-deftest mobile-clipboard-error-restores-terminal ()
  (mobile-clipboard-test-with-routing
   (let ((prior (terminal-parameter nil 'xterm--set-selection)))
     (cl-letf (((symbol-function 'send-string-to-terminal)
                (lambda (&rest _) (error "disconnected terminal"))))
       (kill-new "keep this"))
     (should (equal (car kill-ring) "keep this"))
     (should-not events)
     (should (eq (terminal-parameter nil 'xterm--set-selection) prior)))))

(ert-deftest mobile-clipboard-empty-and-whitespace-policy ()
  (mobile-clipboard-test-with-routing
   (kill-new "   ")
   (should-not events)
   (should-not kill-ring)
   ;; An explicit empty clipboard write can clear it; ordinary whitespace
   ;; kills above retain the existing kill-new policy.
   (night/mobile-clipboard-cut "")
   (should (equal events '("\e]52;c;\a")))))

(ert-run-tests-batch-and-exit)

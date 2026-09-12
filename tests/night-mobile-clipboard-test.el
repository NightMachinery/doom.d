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

(defmacro mobile-clipboard-test-with-ssh (&rest body)
  `(let ((night/h-mobile-clipboard-ssh-cache (make-hash-table :test #'equal))
         (night/h-mobile-clipboard-ssh-jobs (make-hash-table :test #'equal))
         (night/mobile-clipboard-max-bytes 6)
         (clock 1000)
         calls outputs)
     (cl-letf (((symbol-function 'float-time) (lambda (&rest _) clock))
               ((symbol-function 'night/h-mobile-clipboard-ssh-run)
                (lambda (&rest args) (setq calls (append calls (list args)))))
               ((symbol-function 'night/h-mobile-clipboard-osc52-cut)
                (lambda (text) (push text outputs))))
       ,@body)))

(ert-deftest mobile-clipboard-ssh-probes-caches-and-serializes ()
  (mobile-clipboard-test-with-ssh
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "small")
   (should (equal outputs '("small")))
   (should-not calls)
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "large first")
   (should (= (length calls) 1))
   (should-not (plist-get (car calls) :text))
   ;; Replace a pending oversized selection before its readiness probe ends.
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "large latest")
   (funcall (plist-get (car calls) :callback) t)
   (should (= (length calls) 2))
   (should (equal (plist-get (cadr calls) :text) "large latest"))
   (should (eq (night/h-mobile-clipboard-ssh-ready "phone") 'ready))
   ;; A small selection must not be overwritten by the in-flight large one.
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "new")
   (should (equal outputs '("small")))
   (funcall (plist-get (cadr calls) :callback) t)
   (should (equal outputs '("new" "small")))
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "cached large")
   (should (= (length calls) 3))
   (should (equal (plist-get (nth 2 calls) :text) "cached large"))
   (funcall (plist-get (nth 2 calls) :callback) t)
   (setq clock 1899)
   (should (eq (night/h-mobile-clipboard-ssh-ready "phone") 'ready))
   (setq clock 1900)
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "expired large")
   (should-not (plist-get (nth 3 calls) :text))))

(ert-deftest mobile-clipboard-ssh-failure-expiry-and-reset ()
  (mobile-clipboard-test-with-ssh
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "large first")
   (funcall (plist-get (car calls) :callback) nil)
   (should (eq (night/h-mobile-clipboard-ssh-ready "phone") 'down))
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "large dropped")
   (should (= (length calls) 1))
   (setq clock 1029)
   (should (eq (night/h-mobile-clipboard-ssh-ready "phone") 'down))
   (setq clock 1030)
   (should-not (night/h-mobile-clipboard-ssh-ready "phone"))
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "large retry")
   (funcall (plist-get (nth 1 calls) :callback) t)
   (should (equal (plist-get (nth 2 calls) :text) "large retry"))
   ;; A failed transfer invalidates successful readiness immediately.
   (funcall (plist-get (nth 2 calls) :callback) nil)
   (should (eq (night/h-mobile-clipboard-ssh-ready "phone") 'down))
   (night/h-mobile-clipboard-ssh-cache-put :host "other" :ready t)
   (night/mobile-clipboard-cache-clear "phone")
   (should-not (night/h-mobile-clipboard-ssh-ready "phone"))
   (should (eq (night/h-mobile-clipboard-ssh-ready "other") 'ready))
   (night/mobile-clipboard-cache-clear)
   (should (= (hash-table-count night/h-mobile-clipboard-ssh-cache) 0))))

(ert-deftest mobile-clipboard-tealy-cache-clear-is-scoped ()
  (mobile-clipboard-test-with-ssh
   (night/h-mobile-clipboard-ssh-cache-put :host "tealy" :ready t)
   (night/h-mobile-clipboard-ssh-cache-put :host "other" :ready t)
   (should (commandp #'night/mobile-clipboard-tealy-cache-clear))
   (should (call-interactively #'night/mobile-clipboard-tealy-cache-clear))
   (should-not (night/h-mobile-clipboard-ssh-ready "tealy"))
   (should (eq (night/h-mobile-clipboard-ssh-ready "other") 'ready))
   (should (night/mobile-clipboard-tealy-cache-clear))))

(ert-deftest mobile-clipboard-ssh-failure-releases-new-small-copy ()
  (mobile-clipboard-test-with-ssh
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "large first")
   (night/h-mobile-clipboard-ssh-enqueue :host "phone" :text "new")
   (funcall (plist-get (car calls) :callback) nil)
   (should (equal outputs '("new")))
   (should (= (length calls) 1))))

(ert-deftest mobile-clipboard-ssh-frame-opt-in ()
  (let ((prior (frame-parameter nil 'night/clipboard-ssh-host)) calls)
    (unwind-protect
        (cl-letf (((symbol-function 'night/mobile-clipboard-frame-p) (lambda () t))
                  ((symbol-function 'night/h-mobile-clipboard-ssh-enqueue)
                   (lambda (&rest args) (push args calls)))
                  ((symbol-function 'night/h-mobile-clipboard-osc52-cut)
                   (lambda (_text) (push 'osc calls))))
          (set-frame-parameter nil 'night/clipboard-ssh-host "phone")
          (night/mobile-clipboard-cut "text")
          (should (equal calls '((:host "phone" :text "text"))))
          (set-frame-parameter nil 'night/clipboard-ssh-host nil)
          (night/mobile-clipboard-cut "text")
          (should (eq (car calls) 'osc)))
      (set-frame-parameter nil 'night/clipboard-ssh-host prior))))

(ert-deftest mobile-clipboard-ssh-runner-utf8-and-timeout ()
  (let ((real-make-process (symbol-function 'make-process))
        (night/mobile-clipboard-ssh-timeout 2)
        command captured process results)
    ;; Exercise real pipe encoding, EOF, and sentinel behavior without a phone.
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq command (plist-get args :command))
                 (setq args (plist-put args :command '("/bin/cat")))
                 (setq args (plist-put args :filter
                                       (lambda (_proc text)
                                         (setq captured (concat captured text)))))
                 (setq process (apply real-make-process args)))))
      (night/h-mobile-clipboard-ssh-run
       :host "phone" :text "hello فارسی 🦊\n\n"
       :callback (lambda (ok) (push ok results)))
      (let ((deadline (+ (float-time) 3)))
        (while (and (not results) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (should (equal results '(t)))
      (should (equal captured "hello فارسی 🦊\n\n"))
      (should (equal (last command 3) '("--" "phone" "termux-clipboard-set")))
      (should-not (member "hello فارسی 🦊\n\n" command)))
    (setq results nil night/mobile-clipboard-ssh-timeout 0.03)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq process (apply real-make-process
                                      (plist-put args :command '("/bin/sleep" "2")))))))
      (night/h-mobile-clipboard-ssh-run
       :host "phone" :callback (lambda (ok) (push ok results)))
      (let ((deadline (+ (float-time) 1)))
        (while (and (not results) (< (float-time) deadline))
          (accept-process-output nil 0.01)))
      (accept-process-output nil 0.05)
      (should (equal results '(nil)))
      (should-not (process-live-p process)))))

(ert-run-tests-batch-and-exit)

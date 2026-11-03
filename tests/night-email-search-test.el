;;; -*- lexical-binding: t; -*-
;; Run with -L pointing at the pinned notmuch checkout's emacs directory.
(setq native-comp-enable-subr-trampolines nil)
(require 'ert)
(require 'cl-lib)
(require 'notmuch)
(defmacro after! (&rest _) nil)
(defun night/set-leader-keys (&rest _) nil)
(load (expand-file-name "../autoload/night-email.el"
                        (file-name-directory (or load-file-name buffer-file-name))) nil t)

;; What `after! notmuch' in night-email.el sets up.
(setq notmuch-search-line-faces
      '(("flagged" . night/notmuch-search-flagged)
        ("unread" . night/notmuch-search-unread)))
(advice-add 'notmuch-search-append-result :around #'night/h-notmuch-search-hold-result)
(advice-add 'notmuch-search-process-sentinel :before #'night/h-notmuch-search-release-results)

(defconst night-test/results
  '(("a") ("b" "unread") ("c" "flagged") ("d" "unread" "flagged") ("e") ("f" "unread"))
  "Synthetic threads in notmuch's newest-first order: (THREAD TAG...).")

(defun night-test/result (spec)
  (list :thread (car spec) :tags (cons "inbox" (cdr spec)) :matched 1 :total 1
        :date_relative "today" :authors "Someone" :subject (car spec)))

(defun night-test/search (query &optional target)
  "Return a search buffer for QUERY filled as notmuch would fill it.
TARGET is the thread a refresh asks point to stay on."
  (let ((buffer (get-buffer-create (format "*night-test %s*" query))))
    (with-current-buffer buffer
      (notmuch-search-mode)
      (setq notmuch-search-query-string query
            notmuch-search-target-thread target)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (spec night-test/results)
          (notmuch-search-append-result (night-test/result spec))))
      (let ((proc (make-process :name "night-test" :buffer buffer
                                :command '("true") :sentinel #'ignore)))
        (while (process-live-p proc) (accept-process-output proc 0.05))
        (process-put proc 'parse-buf (generate-new-buffer " *night-test parse*"))
        (notmuch-search-process-sentinel proc "finished\n")))
    buffer))

(defun night-test/threads (buffer)
  "Return the thread of each result line in BUFFER, top to bottom."
  (with-current-buffer buffer
    (let (threads)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((result (notmuch-search-get-result)))
            (when result (push (plist-get result :thread) threads)))
          (forward-line 1)))
      (nreverse threads))))

(defun night-test/line-faces (buffer thread)
  "Return the night faces at the start of THREAD's line in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (while (not (equal (plist-get (notmuch-search-get-result) :thread) thread))
        (forward-line 1))
      (let ((face (get-text-property (point) 'face)))
        (seq-filter (lambda (f) (memq f '(night/notmuch-search-flagged night/notmuch-search-unread)))
                    (cond ((listp face) face) (t (list face))))))))

(ert-deftest night-mail-inbox-groups-unread-and-flagged-first ()
  (let ((buffer (night-test/search "tag:inbox")))
    (unwind-protect
        (progn
          (should (equal (night-test/threads buffer) '("d" "b" "f" "c" "a" "e")))
          ;; Flagged's background first, so it wins; unread adds its bold.
          (should (equal (night-test/line-faces buffer "d")
                         '(night/notmuch-search-flagged night/notmuch-search-unread)))
          (should (equal (night-test/line-faces buffer "b") '(night/notmuch-search-unread)))
          (should (equal (night-test/line-faces buffer "c") '(night/notmuch-search-flagged)))
          (should (null (night-test/line-faces buffer "a")))
          (with-current-buffer buffer
            (goto-char (point-max))
            (should (search-backward "End of search results." nil t))))
      (kill-buffer buffer))))

(ert-deftest night-mail-other-searches-keep-date-order ()
  (let ((buffer (night-test/search "from:someone")))
    (unwind-protect
        (should (equal (night-test/threads buffer) '("a" "b" "c" "d" "e" "f")))
      (kill-buffer buffer))))

(ert-deftest night-mail-refresh-keeps-point-on-its-thread ()
  (let ((buffer (night-test/search "tag:inbox" "e")))
    (unwind-protect
        (with-current-buffer buffer
          (should (equal (plist-get (notmuch-search-get-result) :thread) "e")))
      (kill-buffer buffer))))

(ert-deftest night-mail-grouping-toggle-survives-the-mode-reset ()
  (let ((buffer (get-buffer-create "*night-test toggle*")))
    (unwind-protect
        (with-current-buffer buffer
          (notmuch-search-mode)
          (setq notmuch-search-query-string "tag:inbox")
          (should (night/h-notmuch-search-grouped-p))
          (setq night/notmuch-search-grouped nil)
          (notmuch-search-mode)
          (setq notmuch-search-query-string "tag:inbox")
          (should-not (night/h-notmuch-search-grouped-p))
          (setq night/notmuch-search-grouped t
                notmuch-search-query-string "from:someone")
          (should (night/h-notmuch-search-grouped-p)))
      (kill-buffer buffer))))

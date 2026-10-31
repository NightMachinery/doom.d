;;; night-bookmarks-test.el --- Bookmark location policy -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'bookmark)

(defmacro after! (_packages &rest body) `(progn ,@body))
(defmacro comment (&rest _body) nil)

;; Do not load personal bookmarks when loading the module in isolation.
(cl-letf (((symbol-function 'f-exists-p) (lambda (_file) nil)))
  (load (expand-file-name "../autoload/night-bookmarks.el"
                          (file-name-directory load-file-name)) nil t))

(defmacro night-test/with-bookmarks (&rest body)
  "Run BODY with an isolated file buffer and bookmark storage."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "night-bookmarks-" t))
          (file (expand-file-name "document.txt" directory))
          (bookmark-default-file (expand-file-name "bookmarks.el" directory))
          (bookmark-alist nil)
          (bookmark-alist-modification-count 0)
          (bookmark-current-bookmark nil)
          (bookmark-save-flag 1)
          (bookmark-fringe-mark nil)
          (bookmark-use-annotations nil))
     (unwind-protect
         (progn
           (with-temp-file file
             (insert "First paragraph with enough context.\n"
                     "Second paragraph with enough context.\n"
                     "Third paragraph with enough context.\n"))
           (with-current-buffer (find-file-noselect file)
             (goto-char 45)
             ,@body))
       (when-let* ((buffer (get-file-buffer file))) (kill-buffer buffer))
       (delete-directory directory t))))

(ert-deftest night/bookmark-file-has-no-location ()
  (night-test/with-bookmarks
    (bookmark-set "file")
    (let ((record (bookmark-get-bookmark "file")))
      (should (equal (bookmark-get-filename record) file))
      (dolist (property '(position front-context-string rear-context-string))
        (should-not (assq property (cdr record)))))
    ;; Verify the jump works and preserves the existing buffer's point.
    (goto-char 70)
    (bookmark-default-handler (bookmark-get-bookmark "file"))
    (should (= (point) 70))
    ;; Auto-save must write the filtered record, not the original location.
    (let ((saved (with-temp-buffer
                   (insert-file-contents bookmark-default-file)
                   (goto-char (point-min))
                   (read (current-buffer)))))
      (should-not (assq 'position (cdr (assoc "file" saved)))))))

(ert-deftest night/bookmark-interactive-prefix-records-location ()
  (night-test/with-bookmarks
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "interactive")))
      (let ((current-prefix-arg nil)) (call-interactively #'bookmark-set))
      (should-not (bookmark-get-position "interactive"))
      (let ((current-prefix-arg '(4))) (call-interactively #'bookmark-set))
      (should (= (bookmark-get-position "interactive") 45))
      (should (stringp (bookmark-get-front-context-string "interactive")))
      (should (= (length bookmark-alist) 1))
      (goto-char 70)
      (bookmark-default-handler (bookmark-get-bookmark "interactive"))
      (should (= (point) 45)))))

(ert-deftest night/bookmark-position-command-and-replacement ()
  (night-test/with-bookmarks
    (cl-letf (((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "position")))
      (call-interactively #'night/bookmark-set-with-position))
    (should (= (bookmark-get-position "position") 45))
    (bookmark-set "position")
    (should-not (bookmark-get-position "position"))
    (should (= (length bookmark-alist) 1))))

(ert-deftest night/bookmark-programmatic-no-overwrite-and-order ()
  (night-test/with-bookmarks
    (bookmark-set "first")
    (bookmark-set "second")
    (bookmark-set "first" t)
    (should (equal (mapcar #'car bookmark-alist) '("first" "second" "first")))
    (dolist (record bookmark-alist)
      (should-not (bookmark-get-position record)))))

(ert-deftest night/bookmark-special-handler-keeps-location ()
  (night-test/with-bookmarks
    (let ((bookmark-make-record-function
           (lambda ()
             '((filename . "special.txt") (handler . ignore)
               (position . 45) (front-context-string . "context")
               (page . 3)))))
      (bookmark-set "special")
      (should (= (bookmark-get-position "special") 45))
      (should (= (bookmark-prop-get "special" 'page) 3))
      (should (equal (bookmark-get-front-context-string "special") "context")))))

(ert-deftest night/bookmark-standalone-record-generation-keeps-location ()
  (night-test/with-bookmarks
    (should (= (bookmark-get-position (bookmark-make-record)) 45))
    (should (= (alist-get 'position (bookmark-make-record-default)) 45))))

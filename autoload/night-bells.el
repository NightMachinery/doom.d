;;; autoload/night-bells.el -*- lexical-binding: t; -*-
;;;
(defun night/say (text)
  (interactive)
  (z-async t fsay (identity text)))
(comment
 (night/say "hello")
 (night/say "beautiful day!"))
;;;
(defun night/bell-link ()
  (z-async t "tts-glados1-cached" "link, inserted"))

(defun night/bello ()
  (interactive)
  (z-async t bello))

(defun night/bell-fail ()
  (interactive)
  (z-async t bell-fail))

(defun night/bell-pp-ok ()
  (interactive)
  (z-async t bell-pp-ok))

(defun night/bell-lm-ok ()
  (interactive)
  (z-async t bell-lm-ok))

(defun night/bell-export-completed ()
  (interactive)
  (z-async t bell-batman-cave-open))
;;;
(defun night/bell-visual-flash-buffer ()
  "Flash the current buffer with a blue background briefly."
  (interactive)
  (let ((flash-overlay (make-overlay (point-min) (point-max))))
    (overlay-put flash-overlay 'face '(:background
                                       ;; "plum"
                                       ;; "pink"
                                       "MediumPurple1"
                                       :foreground "MintCream"
                                       :extend t))  ; Make overlay extend past text
    (overlay-put flash-overlay 'window (selected-window))  ; Ensure it applies to current window
    (overlay-put flash-overlay 'priority 100)  ; High priority to ensure visibility
    (run-with-timer 0.4 nil #'delete-overlay flash-overlay)))
;;;
(after! (org ox-beamer)
  ;; (advice-add #'org-beamer-export-to-pdf :after #'night/bell-export-completed)

  ;; (night/unadvice #'org-babel-execute-subtree)
  (advice-add #'org-babel-execute-subtree :after #'night/bell-visual-flash-buffer)
  )
;;;

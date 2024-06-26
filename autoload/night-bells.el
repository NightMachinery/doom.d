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
(after! (org ox-beamer)
  ;; (advice-add #'org-beamer-export-to-pdf :after #'night/bell-export-completed)
  )
;;;

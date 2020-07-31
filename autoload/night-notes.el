;;; ~/doom.d/autoload/night-notes.el -*- lexical-binding: t; -*-

(defun night/search-notes ()
  (interactive)
  (night/search-dir (getenv "nightNotes")))
(night/set-leader-keys "z n" #'night/search-notes)

(defun night/browse-notes ()
  (interactive)
  ;; (counsel-find-file (getenv "nightNotes"))
  (counsel-file-jump "" (getenv "nightNotes"))
  ;; fzf seems slower, but it supports fzf syntax and is async
  ;; (counsel-fzf "" (getenv "nightNotes"))
  ;; (dired (getenv "nightNotes"))
)
(night/set-leader-keys " z ." #'night/browse-notes)

(defun night/unt ()
  (interactive)
  (let ((link (current-kill 0)))
    (insert (shell-command-to-string (concat "brishz.dash unt " (shell-quote-argument link))))
    ))

;; (message (shell-command-to-string "brishz.dash unt"))

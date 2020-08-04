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

;;;
(defun night/unt ()
  (interactive)
  (let* ((link (current-kill 0))
         ;; (cmd (concat "brishzr.dash unt " (shell-quote-argument link)))
         (cmd (concat "brishzr.dash " (shell-quote-argument (concat "ec " (shell-quote-argument link) " | inargsf unt"))))
         )
    (message cmd)
    ;; (message link)
    ;; (insert (progn (setenv  "brishzr_in" link) (shell-command-to-string "brishzr.dash inargsf unt")))
    (insert (shell-command-to-string cmd))
    (save-buffer)
    ))

(night/set-leader-keys " z l" #'night/unt)
;; (message (shell-command-to-string "brishz.dash unt"))
;; (message (progn (setenv  "brishzr_in" "haki") (shell-command-to-string "echo $brishzr_in")))
;;; 

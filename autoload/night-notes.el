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
(comment (defun night/unt-async ()
           (interactive)
           (let* (
                  (link (current-kill 0))
                  ;; (my-buffer (current-buffer))
                  )
             (async-start (lambda () (let* (
                                            (cmd (concat "brishzr.dash " (shell-quote-argument (concat "ec " (shell-quote-argument link) " | inargsf unt"))))
                                            (text (shell-command-to-string cmd))
                                            )
                                       text
                                       ))
                          (lambda (text)
                            (message "name: %s my-buffer: %s string: %s" (buffer-name) "my-buffer" (buffer-string))
                            ;; (with-current-buffer my-buffer (insert text))
                            )))))

(defun night/unt ()
  (interactive)
  (let* (
         (my-column (current-column))
         (link (current-kill 0))
         ;; (cmd (concat "brishzr.dash unt " (shell-quote-argument link)))
         (cmd (concat "brishzr.dash " (shell-quote-argument (concat "ec " (shell-quote-argument link) " | inargsf unt"))))
         (text (progn
                 (message "%s" cmd)
                 (shell-command-to-string cmd)))
         )
    (let ((lines (split-string text "\n")))


      (dolist (line lines)
        (progn (if (not (string= "" line)) ; so as to not insert the last empty line
                   (progn (insert line "\n")
                          (dotimes (i my-column)
                            (insert " ")))
                 )))
      )
    (save-buffer)
    ))

(night/set-leader-keys " z l" #'night/unt)
;; (message "%s" (shell-command-to-string "brishz.dash unt"))
;; (message "%s" (progn (setenv  "brishzr_in" "haki") (shell-command-to-string "echo $brishzr_in")))
;;; 

;;; ~/doom.d/autoload/night-notes.el -*- lexical-binding: t; -*-

(defun night/search-notes ()
  (interactive)
  (night/search-dir (getenv "nightNotes")))
(night/set-leader-keys "z n" #'night/search-notes)

;;;
(defun night/browse-dir (dir)
  (interactive)
  (let ((default-directory "/"))
    ;; (counsel-find-file dir)
    ;; (counsel-file-jump "" dir) ; we used this one before using fzf
    (let ((counsel-fzf-cmd (concat "env fzf_mru_minquery=5 fzf_mru_iteration_count=1 fzf_mru_nostdin=y fzf_mru_context=" (shell-quote-argument dir) " fzf_mru.sh --tiebreak=end,length -f \"%s\"")))
      ;; @FR Make counsel-fzf sort the entries it feeds to fzf by MRU https://github.com/abo-abo/swiper/issues/2832
      (counsel-fzf "" dir ""))
    ;; fzf seems slower, but it supports fzf syntax and is async
    ;; (counsel-fzf "" dir)
    ;; (dired dir)
    ))

(defun night/browse-notes ()
  (interactive)
  (night/browse-dir (getenv "nightNotes"))
  )
(night/set-leader-keys " z ." #'night/browse-notes)

(defun night/browse-NIGHTDIR ()
  (interactive)
  (night/browse-dir (getenv "NIGHTDIR"))
  )

(defun night/browse-DOOMDIR ()
  (interactive)
  (night/browse-dir (getenv "DOOMDIR"))
  )
;;;
(comment (defun night/unt-async ()
           (interactive)
           (let* (
                  (link (current-kill 0))
                  ;; (my-buffer (current-buffer))
                  )
             (async-start (lambda () (let* (
                                            (cmd (concat "brishzr.dash " (shell-quote-argument (concat "ec " (shell-quote-argument link) " | inargsf @opts emacs y @ unt"))))
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
         (default-directory "/")
         (my-column (current-column))
         (link (current-kill 0))
         (cmd (concat "brishzr.dash " (shell-quote-argument (concat "ec " (shell-quote-argument link) " | inargsf @opts emacs y @ unt")))) ; @placeholder use brishzr
         (text (progn
                 (message "%s" cmd)
                 (shell-command-to-string cmd))))
    (let ((lines (split-string text "\n" t "\s+")))
      (let ((link (car lines))
            (meta (cdr lines))
            (i 0))
        (insert-for-yank link)
        (dolist (line meta)
          (progn (if (not (string= "" line)) ; so as to not insert the last empty line
                     (progn
                       (setq i (+ i 1))
                       (cond
                        ((= i 1)
                         (org-insert-subheading nil))
                        (t
                         (org-insert-heading nil)))
                       (insert-for-yank line)
                       ;; (insert line "\n" (make-string my-column ?\s))
                                        ; ?\s is the character for space.
                       ))))))
    (save-buffer)
    ;; (night/brishz  "awaysh" "ot-play-beeps1" "3")
    (night/brishz "awaysh" "tts-glados1-cached" "link, inserted")
    ))

(defun night/tmp ()
  (interactive)
  (dolist (i '(1 2 3))
    (org-insert-subheading nil)
    (insert-for-yank "hello"))
  )
;;;
;; (night/set-leader-keys " z l" #'night/unt)
(map! :map org-mode-map
      :localleader
      :nvi "lu" #'night/unt
      )
;;;
;; (message "%s" (shell-command-to-string "brishz.dash unt"))
;; (message "%s" (progn (setenv  "brishzr_in" "haki") (shell-command-to-string "echo $brishzr_in")))
;;; 

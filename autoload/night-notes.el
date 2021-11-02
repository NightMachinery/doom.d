;;; ~/doom.d/autoload/night-notes.el -*- lexical-binding: t; -*-

(defun night/search-notes (&optional initial-query)
  (interactive)
  (night/search-dir :dir (getenv "nightNotes") :query initial-query :args "--glob *.{org,org_archive,md,zsh,txt}"))

(defun night/agsi (&optional initial-query)
  (interactive)
  (night/search-dir :dir (getenv "NIGHTDIR") :query initial-query :args "--glob *"))

(defun night/search-doom (&optional initial-query)
  (interactive)
  (night/search-dir :dir (getenv "DOOMDIR") :query initial-query :args "--glob *"))

(defun night/search-r_rational (&optional initial-query)
  (interactive)
  (night/search-dir :dir "/Volumes/hyper-diva/archives/reddit/rational/posts/" :query initial-query :args "--glob *"))

(night/set-leader-keys "z n" #'night/search-notes)

;;;
(defun night/browse-dir (&optional dir)
  (interactive)
  (let* ((ivy-truncate-lines nil)
         (dir (or dir default-directory "/"))
         (default-directory dir))
    ;; (counsel-find-file dir)
    ;; (counsel-file-jump "" dir) ; we used this one before using fzf

    ;;;
    (let ((counsel-fzf-cmd
           (concat
            "env FORCE_NONINTERACTIVE=y FZF_DEFAULT_COMMAND=\"fd --no-ignore --hidden --exclude=.git --follow\" fzf_mru_minquery=5 fzf_mru_iteration_count=1 fzf_mru_nostdin=y fzf_mru_context=" (shell-quote-argument dir)
            " fzf"
            ;; " fzf_mru.sh"
            " --tiebreak=end,length -f \"%s\"")))
      ;; @FR Make counsel-fzf sort the entries it feeds to fzf by MRU https://github.com/abo-abo/swiper/issues/2832
      (counsel-fzf "" dir ""))
    ;;;
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

(defun night/browse-HOME ()
  (interactive)
  (let (
        (default-directory "/")
        (dir (getenv "HOME"))
        (counsel-fzf-cmd (concat "env FZF_DEFAULT_COMMAND='fd -uu --max-depth=3' " counsel-fzf-cmd))
        )
    ;; (night/browse-dir dir)
    (counsel-fzf "" dir "")
    )
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

(cl-defun night/unt (&key (description t) (quote t))
  (interactive)
  (let*
      (
       (default-directory "/")
       (was-normal (evil-normal-state-p))
       (starting-level (or (org-current-level) 1))
       (links
        (z reval-true pbpaste-urls)
        ;; (current-kill 0)
        )                             ; @todo2 with prefix arg, use clipboard-fz
       (links (split-string links "\n" t "\s+"))
       (links-i 0)
       (links-len (length links)))
    (dolist (link links)
      (setq links-i (+ links-i 1))
      (let* (
             (my-column (current-column))
             (cmd (concat "brishzr.dash " (shell-quote-argument (concat "ec " (shell-quote-argument link) " | inargsf serr @opts emacs y @ unt"))))
             (text (progn
                     (message "%s" cmd)
                     (shell-command-to-string cmd))))
        (let ((lines (split-string text "\n" t "\s+")))
          (let ((link (car lines))
                (meta (cdr lines))
                (i 0))
            (when (not (equalp links-i 1))
              (night/+org--insert-item 'below
                                       starting-level
                                       "\n"))
            (insert-for-yank link)
            (if (and description meta)
                (if quote
                    (progn
                      (insert (format  "\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" meta)))
                    (dolist (line meta)
                      (progn (if (not (string= "" line)) ; so as to not insert the last empty line
                                 (progn
                                   (setq i (+ i 1))
                                   (cond
                                    ((= i 1)
                                     ;; (org-insert-subheading nil)
                                     (night/+org--insert-item 'below (+ (or (org-current-level) 1) 1)))
                                    (t
                                     ;; (org-insert-heading nil)
                                     (+org--insert-item 'below)))
                                   (insert-for-yank line)
                                   ;; (insert line "\n" (make-string my-column ?\s))
                                        ; ?\s is the character for space.
                                   ))))))))))
    (save-buffer)
    (when was-normal
      (evil-normal-state t))
    ;; (night/brishz  "awaysh" "ot-play-beeps1" "3")
    (night/bell-link)))

(defun night/url-fanficfare-insert ()
  (interactive)
  (let*
      (
       (default-directory "/")
       (was-normal (evil-normal-state-p))
       (starting-level (or (org-current-level) 1))
       (links
        (z reval-true pbpaste-urls))
       (links (split-string links "\n" t "\s+"))
       (links-i 0)
       (links-len (length links)))
    (dolist (link links)
      (setq links-i (+ links-i 1))
      (let* (
             (text (z fanficfare2org (i link))))
        (when (not (equalp links-i 1))
          (night/+org--insert-item 'below
                                   starting-level
                                   "\n"))
        (insert-for-yank text)
        (redisplay)))
    (save-buffer)
    (when was-normal
      (evil-normal-state t))
    (night/bell-link)))

(defun night/bell-link ()
  (night/brishz "awaysh" "tts-glados1-cached" "link, inserted"))

(defun night/after-link (&rest args)
  (save-buffer)
  (night/bell-link))

(defun night/tmp ()
  (interactive)
  (dolist (i '(1 2 3))
    ;; (org-insert-subheading nil)
    (+org--insert-item 'below)
    (insert-for-yank "hello"))
  )
;;;
;; (night/set-leader-keys " z l" #'night/unt)
(map! :map org-mode-map
      :localleader
      :nvi "lu" #'night/unt

      :nvi "lf" #'night/url-fanficfare-insert)
;;;
;; (message "%s" (shell-command-to-string "brishz.dash unt"))
;; (message "%s" (progn (setenv  "brishzr_in" "haki") (shell-command-to-string "echo $brishzr_in")))
;;; 
(defun night/notes-private-file-p (file)
  (progn
    (s-matches? "\\.\\{2\\}private\\.\\{2\\}" file)))
;;;

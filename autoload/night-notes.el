;;; ~/doom.d/autoload/night-notes.el -*- lexical-binding: t; -*-

(defun night/search-notes (&optional initial-query)
  (interactive)
  (night/search-dir
   :engine "ug"

   :dir (getenv "nightNotes")

   :query initial-query
   :include-extensions '("org" "org_archive" "md" "tex" "txt" "json" "csv" "zsh" "py")
   :exclude-globs '("*chrome*bookmarks.org"))
  ;; (night/search-dir-consult
  ;;  :dir (getenv "nightNotes") :query initial-query
  ;;  :args "--glob=*.{org,org_archive,md,tex,txt,json,csv,zsh,py} --glob=!*chrome*bookmarks.org")
  )

(cl-defun night/search-research
    (&key
     (query nil
))
  (interactive)
  (night/search-dir-limited
   :dir (concat
         (getenv "nightNotesPublic")
         "/subjects/math/")
   :extra-paths (list
                 (concat
                  (getenv "nightNotesPublic")
                  "/subjects/ML/")
                 (concat
                  (getenv "nightNotesPrivate")
                  "/subjects/supervisors/")
                 (concat
                  (getenv "nightNotesPrivate")
                  "/research/"))
   :query query))

(defun night/search-research-CR ()
  (interactive)
  (night/search-research
   :query "@CR\\ "))

(defun night/search-ideas ()
  (interactive)
  ;; We need the ugrep engine for =-pat= to work.
  ;; We need the ivy-rg engine for =!pat= to work.
  (night/search-research
   :query
   "IDEA|@idea -@ideas -@idea/rejected -@idea/bad -@idea/presented -@idea/costly -@idea/small"
   ;; "IDEA\\|@idea !@ideas !@idea/rejected !@idea/bad !@idea/presented !@idea/costly !@idea/small"
   ))

(defun night/search-ideas-accepted ()
  (interactive)
  (night/search-research
   :query "@idea/accepted"))

(cl-defun night/search-dir-limited
    (&key
     (dir nil)
     (extra-paths nil)
     (args "")
     (query "")
     (prompt nil)
     (engine "ug")
     )
  (interactive)
  (night/search-dir-consult
   :dir dir
   :extra-paths extra-paths
   :prompt prompt
   :query query
   :include-extensions '("org" "org_archive" "md" "tex" "txt" "zsh" "py")
   ;; :engine "rg"
   :engine engine
   ))

(defun night/agsi (&optional initial-query)
  (interactive)
  (night/search-dir-consult
   :dir (getenv "NIGHTDIR")
   :extra-paths (-filter
                 (lambda (x)
                   (f-exists-p x))
                 (cl-map
                  'list
                  #'expand-file-name
                  ;; @duplicateCode/9bd35fded6a091a2124673d5d0ec11b2
                  ;; [agfi:agsi]
                  '("~/.zshenv"
                    "~/.zshrc"
                    "~/.shared.sh"
                    "~/.localScripts"
                    "~/.glances"
                    "~/.vimrc"
                    "~/.ideavimrc"
                    "~/.tmux.conf"
                    "~/.privateBTT.sh"
                    "~/.privateShell"
                    "~/.privateStartup.sh"
                    "~/.startup..private..zsh"
                    "~/.hammerspoon/init.lua"
                    "~/test_nonexistent")))
   :query initial-query :args "--glob=*"))

(defun night/search-doom (&optional initial-query)
  (interactive)
  (night/search-dir :dir (getenv "DOOMDIR") :query initial-query :args "--glob=*"))

(defun night/search-r_rational (&optional initial-query)
  (interactive)
  (night/search-dir :dir "/Volumes/hyper-diva/archives/reddit/rational/posts/" :query initial-query :args "--glob=*"))

(night/set-leader-keys "z n" #'night/search-notes)

;;;
(defun night/browse-dir (&optional dir query)
  (interactive)
  (let* ((ivy-truncate-lines nil)
         (dir (or dir default-directory "/"))
         (default-directory dir)
         (query (or query "")))
    ;; (counsel-find-file dir)
    ;; (counsel-file-jump "" dir) ; we used this one before using fzf

    ;;;
    (let ((counsel-fzf-cmd
           (concat
            "env FORCE_NONINTERACTIVE=y FZF_DEFAULT_COMMAND=\"fd --no-ignore --hidden --exclude=.git --follow\" fzf_mru_minquery=5 fzf_mru_iteration_count=1 fzf_mru_nostdin=y fzf_mru_context=" (shell-quote-argument dir)
            " fzf"
            ;; " fzf_mru.sh" ;; @disabled
            " --tiebreak=length,begin -f \"%s\"")))
      ;; --tiebreak=end,length
      ;; @FR Make counsel-fzf sort the entries it feeds to fzf by MRU https://github.com/abo-abo/swiper/issues/2832
      (counsel-fzf query dir ""))
    ;;;
    ))

(defun night/browse-notes (&optional dir)
  (interactive)
  (let ((note-dir (or dir (getenv "nightNotes"))))
    (night/browse-dir note-dir ".org$ | _archive$ | .gpg$ | .tex$ | .md$ | .txt$ | .json$ ")))
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
;;;
(defun night/html-unescape (text)
  "`+web-decode-entities' seems broken"
  (z html-unescape
     (identity text)))
(comment
 (night/html-unescape "Alice&#x27s Message, Bob's")
 (+web-decode-entities "Alice&#x27s Message, Bob's")
 (org-cliplink-escape-html4 "Alice&#x27s Message, Bob's") ;; currently overridden by advice
 )

(advice-add 'org-cliplink-escape-html4 :override #'night/html-unescape)
;;;
(cl-defun night/unt (&key (description t) (quote t))
  "@alt org-cliplink"
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
                (meta (s-join "\n" (cdr lines)))
                (i 0))
            (when (not (equalp links-i 1))
              (night/+org--insert-item 'below
                                       starting-level
                                       "\n"))
            (insert-for-yank link)
            (if (and description meta)
                (if quote
                    (progn
                      (insert (format "\n#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" meta)))
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

(night/defun-named night/url-fanficfare-insert ()
  (interactive)
  (night/brishz-async-insert
   :name $0
   :command (list "reval-timeout" night/zsh-timeout-default "reval-paste" "fanficfare2org-emc")
   :callback-after #'night/bell-link
   :insert-fn #'night/org-insert-and-fix-levels
   :save-p t))

(defun night/url-fanficfare-insert-sync ()
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

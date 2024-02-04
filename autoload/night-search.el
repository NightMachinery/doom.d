;;; ~/doom.d/autoload/night-search.el -*- lexical-binding: t; -*-

(after! rg
  (rg-enable-menu))
;;;
(defvar night/advice-ivy-calling-enabled-p t
  "Control whether `ivy-calling' is enabled in advice.
This variable can be bound dynamically.")
(setq night/advice-ivy-calling-enabled-p 'no-arg)

(defun night/h-advice-ivy-calling (orig-fun &rest args)
  "Advice to temporarily set `ivy-calling' to `night/advice-ivy-calling-enabled-p' around other functions."
  (let ((ivy-calling-tmp (default-value 'ivy-calling)))
    (setq-default ivy-calling
          (cond
           ((eq night/advice-ivy-calling-enabled-p 'no-arg)
            (not
             ;; C-u should not be present to enable ivy-calling:
             (= (prefix-numeric-value current-prefix-arg) 4)))
           (t night/advice-ivy-calling-enabled-p))
          )
    (unwind-protect
        (apply orig-fun args)
      (setq-default ivy-calling ivy-calling-tmp))))

(defun night/M-x-no-ivy-calling ()
  "Call `counsel-M-x' with `ivy-calling' disabled."
  (interactive)
  (let ((night/advice-ivy-calling-enabled-p nil))
    (counsel-M-x)))

(advice-add '+ivy/project-search :around #'night/h-advice-ivy-calling)
;;;
;; @retired This might only work in "project" dirs. We might need to wrap around counsel-rg directly.
(cl-defun night/search-dir
    (&key
     (dir nil)
     (extra-paths nil)
     (args " --glob !*.ipynb ")
     (query "")
     (prompt "> "))
  "Conduct a text search in files under the given folder."
  (interactive)
  (let* ((default-directory (or dir
                                ;; (counsel--git-root)
                                default-directory))
         (dir default-directory)
         (args
          (concat args " --hidden --glob !.git "))
         (args
          (if extra-paths
              ;; https://github.com/abo-abo/swiper/issues/2356#issuecomment-596277828
              (concat args " -- "
                      (s-join " "
                              (if (equalp dir "/")
                                  extra-paths ;; The root path / is assumed to mean only search extra-paths.
                                (cons dir extra-paths))))
            args)))
    (progn
      ;; (message "args: %s" args)
      (counsel-rg query dir args prompt)
;;;
      ;; (call-interactively
      ;;  (cond
      ;;   ((featurep! :completion ivy) #"+ivy/project-search-from-cwd)
      ;;   ((featurep! :completion helm) #'+helm/project-search-from-cwd)
      ;;   (#'rgrep)))
      )))
(advice-add 'night/search-dir :around #'night/h-advice-ivy-calling)
;;;
;; (setq-default ivy-calling t)
(setq-default ivy-calling nil)
;; t makes ivy follow its results but it had some issues for me before:
;; - flickering (seems solved) https://github.com/abo-abo/swiper/issues/2577
;; - breaks my fzf-M-x
;; - has some occasional bugs, e.g., when scrolling quickly
;; - makes emacs hang-y when it tries to preview a bad (e.g., big) file
;;;
(map! :map ivy-minibuffer-map ;; counsel-ag-map
      ;; "=" #'ivy-call-and-recenter
      "M-/" #'ivy-call-and-recenter
      "C-l" #'ivy-call-and-recenter     ;; Sth is overriding this
      )

(after! deadgrep
  (map! :map deadgrep-mode-map


        :n "a" #'deadgrep-backward-match
        :n "d" #'deadgrep-forward-match
        :n "o" #'deadgrep-visit-result-other-window
        :n "g r" #'deadgrep-restart)
  ;; (define-key deadgrep-mode-map (kbd "gr") #'deadgrep-restart)

  ;; it's a frame popup so window functions don't work
  ;; (add-hook 'deadgrep-mode-hook #'doom/window-maximize-buffer)

  ;; (setq-default deadgrep--context (cons 3 3))
  ;; enabled globally
  ;; (add-hook 'deadgrep-mode-hook #'zoom-mode)
  (defun night/deadgrep-hook ()
    (interactive)
    (progn
      (+word-wrap-mode)
      (next-error-follow-minor-mode)
      ;; these two hang emcrg; without them we need to move first to activate preview.
      ;; (next-error-no-select)
      ;; (evil-next-line)
      ;; (evil-previous-line)
      ))
  (add-hook 'deadgrep-mode-hook #'night/deadgrep-hook)
  (night/set-leader-keys "z d" #'deadgrep)
  ;; deadgrep has some DATA DESTROYING bugs
  ;; a possible workaround
  (defun dotfiles--deadgrep--find-file-wrapper (orig-fun &rest args)
    (save-match-data
      (apply orig-fun args)))
  (advice-add 'deadgrep--find-file :around #'dotfiles--deadgrep--find-file-wrapper)
  )

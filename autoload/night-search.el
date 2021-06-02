;;; ~/doom.d/autoload/night-search.el -*- lexical-binding: t; -*-

(after! rg
  (rg-enable-menu))

;; @retired This might only work in "project" dirs. We might need to wrap around counsel-rg directly.
(cl-defun night/search-dir (&key (dir nil) (args "") (query "") (prompt "> "))
  "Conduct a text search in files under the given folder."
  (interactive)
  (let ((default-directory (or dir default-directory))
        )
    (progn
      (setq ivy-calling-tmp ivy-calling)
      (setq-default ivy-calling t)
      (unwind-protect
          (counsel-rg query dir args prompt)
          ;;;
          ;; (call-interactively
          ;;  (cond
          ;;   ((featurep! :completion ivy) #"+ivy/project-search-from-cwd)
          ;;   ((featurep! :completion helm) #'+helm/project-search-from-cwd)
          ;;   (#'rgrep)))
        (setq-default ivy-calling ivy-calling-tmp)))))

;;;
;; (setq-default ivy-calling t)
(setq-default ivy-calling nil)
;; t makes ivy follow its results but it had some issues for me before:
;; - flickering (seems solved) https://github.com/abo-abo/swiper/issues/2577
;; - breaks my fzf-M-x
;; - has some occasional bugs, e.g., when scrolling quickly
;; - makes emacs hang-y when it tries to preview a bad (e.g., big) file
;; Reemeber that we have mapped `=` to ivy-call
;;;
(map! :map ivy-minibuffer-map ;; counsel-ag-map
      "=" #'ivy-call-and-recenter
      "C-l" #'ivy-call-and-recenter)

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

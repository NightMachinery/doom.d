;;; night-org-rifle.el ---                           -*- lexical-binding: t; -*-
(use-package! helm-org-rifle
  :after org
  ;; :general
  ;; (:keymaps 'org-mode-map
  ;;           :states 'normal
  ;;           :prefix "SPC"
  ;;           "m r" '(:ignore t :wk "Rifle (Helm)")
  ;;           "m r b" '(helm-org-rifle-current-buffer :wk "Rifle buffer")
  ;;           "m r e" '(helm-org-rifle :wk "Rifle every open buffer")
  ;;           "m r d" '(helm-org-rifle-directory :wk "Rifle from org-directory")
  ;;           "m r a" '(helm-org-rifle-agenda-files :wk "Rifle agenda")
  ;;           "m r o" '(:ignore t :wk "Occur (Persistent)")
  ;;           "m r o b" '(helm-org-rifle-current-buffer :wk "Rifle buffer")
  ;;           "m r o e" '(helm-org-rifle :wk "Rifle every open buffer")
  ;;           "m r o d" '(helm-org-rifle-directory :wk "Rifle from org-directory")
  ;;           "m r o a" '(helm-org-rifle-agenda-files :wk "Rifle agenda")
  ;;           )
  )

(after! helm-org-rifle

  (setq helm-org-rifle-directories-recursive t)
  (defun night/rifle-notes ()
    (interactive)
    ;; @broken `apply: Creating pipe: Too many open files` https://github.com/alphapapa/org-rifle/issues/60
    (helm-org-rifle-directories (getenv "nightNotes")))

  (defun night/rifle-pwd ()
    (interactive)
    (helm-org-rifle-directories default-directory))
  )

;;; this needs to be first, otherwise any error in our config will reset recentf
(after! recentf
  ;; (customize-set-value 'recentf-auto-cleanup 3600) ; doesn't work
  (customize-set-variable 'recentf-auto-cleanup 3600)
  (setq recentf-auto-cleanup 3600)
  (setq recentf-max-saved-items 5000)
  )
;;;
(defun load-path-gitmodules (file)
  (add-to-list 'load-path (concat (getenv "DOOMDIR") "/" "gitmodules/" file)))
(defun load-gitmodules (file)
  (load! (concat (getenv "DOOMDIR") "/" "gitmodules/" file)))
(defun load-night (file)
  (load! (concat (getenv "DOOMDIR") "/" "night-" file ".el")))
(setq nightal-dir (concat (getenv "DOOMDIR") "/" "autoload"))
;;;


(defun night/load-config ()
  (interactive)
  (require 'f)
  (require 'dash)
  (mapcar #'load-night '("doom-overrides" "macros" "basic" "doom-keybindings" "gui" "macos-gui"))
  (load-gitmodules "osx-clipboard-mode/osx-clipboard.el")
  (load-gitmodules "fzf.el/fzf.el")
  (mapcar #'load (directory-files-recursively nightal-dir "\.el$"))
  (load "~/.private-config.el" t)
  (progn ;; with-eval-after-load 'pdf-view
    (load-gitmodules "pdf-continuous-scroll-mode.el/pdf-continuous-scroll-mode.el"))
  (night/brishz "bell-sc2-evo-perfection")
  )
(night/load-config)

(defun night/loaded-p ()
  (and (boundp 'night-loaded) night-loaded))
;;;
(defun load-path-gitmodules (file)
  (add-to-list 'load-path (concat (getenv "DOOMDIR") "/" "gitmodules/" file)))
(defun load-gitmodules (file)
  (load! (concat (getenv "DOOMDIR") "/" "gitmodules/" file)))
(defun load-night (file)
  (load! (concat (getenv "DOOMDIR") "/" "night-" file ".el")))
(setq nightal-dir (concat (getenv "DOOMDIR") "/" "autoload"))
;;;

(defun night/server-p ()
  ;; (not (equalp user-login-name "evar"))
  (not (eq system-type 'darwin))
  )

(defun night/load-last ()
  ;; autoloaded functions can still get loaded after this; Use:
  ;; (advice-add 'original-function :override #'something-fixed)
  ;;;
  ;; (mapcar #'load '("autoload/night-completion.el"))
  (mapcar #'load-night '("doom-overrides" "last")))

(defun night/load-truly-last ()
  (mapcar #'load-night '("truly-last")))

(defun night/load-config ()
  (interactive)
  (message "%s" "night/load-config started ...")
  (require 'f)
  (require 'a)
  (require 'dash)
  (require 'memoize)
  (mapcar #'load-night '("brish" "macros" "basic" "doom-keybindings" "gui" "macos-gui"))

  (load-gitmodules "osx-clipboard-mode/osx-clipboard.el")
  (load-gitmodules "fzf.el/fzf.el")
  ;; (load-gitmodules "emacswiki/HighLight.el")

  (mapcar #'load (directory-files-recursively nightal-dir "\.el$"))
  (load "~/.private-config.el" t)
  (progn ;; with-eval-after-load 'pdf-view
    (when (not (night/server-p))
      ;; (load-gitmodules "pdf-continuous-scroll-mode.el/pdf-continuous-scroll-mode.el")
   ))

  (add-hook 'window-setup-hook #'night/load-truly-last)
  (night/load-last)

  (message "TERM: %s" (getenv "TERM"))
  (night/brishz 'awaysh 'eval "sleep 20 ; bell-sc2-evo-perfection")
  )

(cond
 ((file-exists-p (getenv "DOOMDIR"))
  (night/load-config))
 (t (message "DOOMDIR does not exist, skipped night/load-config")))

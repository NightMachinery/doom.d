;;; this needs to be first, otherwise any error in our config will reset recentf
(require 'recentf)
(after! recentf
  ;; (customize-set-value 'recentf-auto-cleanup 3600) ; doesn't work
  (customize-set-variable 'recentf-auto-cleanup 3600)
  (setq recentf-auto-cleanup 3600)
  (setq recentf-max-saved-items 5000)
  (recentf-load-list)
  )
;;;
(defmacro mycomment (&rest a)
  t)
(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)
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
  (mapcar #'load-night '("doom-overrides" "last")))

(defun night/load-config ()
  (interactive)
  (message "%s" "night/load-config started ...")
  (require 'f)
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
      (load-gitmodules "pdf-continuous-scroll-mode.el/pdf-continuous-scroll-mode.el")))

  ;; (add-hook 'doom-after-init-modules-hook #'night/load-last)
  (night/load-last)


  (night/brishz 'awaysh 'eval "sleep 10 ; bell-sc2-evo-perfection")
  )
(night/load-config)

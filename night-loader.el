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
  (mapcar #'load-night '("doom" "macros" "basic" "doom-keybindings" "gui" "macos-gui" "python"))
  (load-gitmodules "osx-clipboard-mode/osx-clipboard.el")
  (mapcar #'load (directory-files-recursively nightal-dir "\.el$"))
  (load "~/.private-config.el" t)
  )
(night/load-config)

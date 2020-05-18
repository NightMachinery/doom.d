(load "~/.private-config.el")
;;;
(defun load-gitmodules (file)
  (load! (concat (getenv "DOOMDIR") "/" "gitmodules/" file)))
(defun load-night (file)
  (load! (concat (getenv "DOOMDIR") "/" "night-" file ".el")))
(setq nightal-dir (concat (getenv "DOOMDIR") "/" "autoload"))
;;;

(load-gitmodules "osx-clipboard-mode/osx-clipboard.el")
(mapcar #'load-night '("macros" "basic" "doom-keybindings" "gui" "macos-gui"))
(mapcar #'load (directory-files nightal-dir 't "\.el$" 't))

(load "~/.private-config.el")
;;;
(defun load-path-gitmodules (file)
  (add-to-list 'load-path (concat (getenv "DOOMDIR") "/" "gitmodules/" file)))
(defun load-gitmodules (file)
  (load! (concat (getenv "DOOMDIR") "/" "gitmodules/" file)))
(defun load-night (file)
  (load! (concat (getenv "DOOMDIR") "/" "night-" file ".el")))
(setq nightal-dir (concat (getenv "DOOMDIR") "/" "autoload"))
;;;

(mapcar #'load-night '("doom" "macros" "basic" "doom-keybindings" "gui" "macos-gui" "python"))
(load-gitmodules "osx-clipboard-mode/osx-clipboard.el")
(mapcar #'load (directory-files nightal-dir 't "\.el$" 't))

;;; autoload/night-dired.el -*- lexical-binding: t; -*-

(require 'dired+)

(after! dired+
  (unbind-key "M-DEL" dired-mode-map)
  (unbind-key "M-O" dired-mode-map)
  (unbind-key "M-<left>" dired-mode-map)
  (unbind-key "M-<right>" dired-mode-map)

  (map! :map dired-mode-map
        :nv "Y" #'diredp-copy-abs-filenames-as-kill
        :nv "{" #'diredp-copy-abs-filenames-as-kill-recursive
        ))

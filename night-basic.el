;;; ~/doom.d/night-basic.el -*- lexical-binding: t; -*-
(setenv "SHELL" "/bin/bash")
(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash") ;;Set emacs shell to bash.
(setenv "PAGER" "cat")                      ;;Required for shell mode.
(defun night/set-leader-keys (keys fn &optional desc)
  (map! :leader
        :desc (or desc (format "%s" fn) "no description provided") keys fn))

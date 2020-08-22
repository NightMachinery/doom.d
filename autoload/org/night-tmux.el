;;; autoload/org/night-tmux.el -*- lexical-binding: t; -*-

;;; Use https://github.com/jackkamm/ob-emamux instead
;;; Screen: https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-screen.html
(use-package! ob-tmux
  :custom
  (org-babel-default-header-args:tmux
   '((:results . "silent")         ;
     (:session . "default")        ; The default tmux session to send code to
     (:socket  . nil)))            ; The default tmux socket to communicate with
  ;; The tmux sessions are prefixed with the following string.
  ;; You can customize this if you like.
  (org-babel-tmux-session-prefix "ob-")
  ;; The terminal that will be used.
  ;; You can also customize the options passed to the terminal.
  ;; The default terminal is "gnome-terminal" with options "--".
  (org-babel-tmux-terminal "xterm")
  (org-babel-tmux-terminal-opts '("-T" "ob-tmux" "-e"))
  ;; Finally, if your tmux is not in your $PATH for whatever reason, you
  ;; may set the path to the tmux binary as follows:
  ;; (org-babel-tmux-location "/usr/bin/tmux")
  )

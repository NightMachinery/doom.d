;;; ~/doom.d/night-doom-keybindings.el -*- lexical-binding: t; -*-

(map! :m "\\" nil)
(map!
 :ng "TAB" #'indent-for-tab-command)
(setq doom-localleader-key "\\")
(setq doom-localleader-alt-key "M-\\")
(map! :leader
      :desc "M-x" "SPC" #'night/fzf-M-x ;; #'execute-extended-command
      )

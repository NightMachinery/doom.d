;;; ~/doom.d/night-doom-keybindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "\\") nil)
(setq doom-localleader-key "\\")
(setq doom-localleader-alt-key "M-\\")
(map! :leader
      :desc "M-x"                   "SPC"    #'execute-extended-command)


;;; ~/doom.d/night-doom-keybindings.el -*- lexical-binding: t; -*-

(map! :m "\\" nil)
(map!
 :ng "TAB" #'indent-for-tab-command)
(setq doom-localleader-key "\\")
(setq doom-localleader-alt-key "M-\\")
(map! :leader
      :desc "M-x"                   "SPC"    #'execute-extended-command)


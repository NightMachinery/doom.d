;;; ~/doom.d/night-doom-keybindings.el -*- lexical-binding: t; -*-

(map! :m "\\" nil)
(map!
 :ng "TAB" #'indent-for-tab-command)
(setq doom-localleader-key "\\")
(setq doom-localleader-alt-key "M-\\")
(map! :leader
      :desc "M-x" "SPC" #'night/fzf-M-x
      ;; :desc "M-x" "SPC" #'execute-extended-command
      )
(comment (map!
  :ng "M-x" #'night/fzf-M-x ;; #'execute-extended-command
  ))
;;;
(define-key input-decode-map "\e[1;10A" [S-M-up])
(define-key input-decode-map "\e[1;10B" [S-M-down])
(define-key input-decode-map "\e[1;10C" [S-M-right])
(define-key input-decode-map "\e[1;10D" [S-M-left])
;;;

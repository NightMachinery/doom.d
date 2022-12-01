;;; ~/doom.d/night-doom-keybindings.el -*- lexical-binding: t; -*-

(map! :m "\\" nil)
(map!
 :ng "TAB" #'indent-for-tab-command)
(setq doom-localleader-key "\\")
(setq doom-localleader-alt-key "M-\\")
(map! :leader
      ;; :desc "M-x" "SPC" #'night/fzf-M-x

      :desc "M-x" "SPC" #'execute-extended-command
      )
(comment (map!
  :ng "M-x" #'night/fzf-M-x ;; #'execute-extended-command
  ))
;;;
(defun night/setup-input-decode ()
  (progn
    (comment
     ;; already supported by emacs
     (define-key input-decode-map "\e[1;2D" [S-left])))
  (comment
   ;; @? iTerm
   (define-key input-decode-map "\e[1;10A" [S-M-up])
   (define-key input-decode-map "\e[1;10B" [S-M-down])
   (define-key input-decode-map "\e[1;10C" [S-M-right])
   (define-key input-decode-map "\e[1;10D" [S-M-left])))

(night/setup-input-decode)

;;* input-decode-map is reset for each terminal instance.
(add-hook 'tty-setup-hook #'night/setup-input-decode)
;;;

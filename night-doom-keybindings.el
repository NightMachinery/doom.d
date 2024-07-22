;;; ~/doom.d/night-doom-keybindings.el -*- lexical-binding: t; -*-

(map! :m "\\" nil)
(map!
 :ng "TAB" #'indent-for-tab-command)
(progn
 (map! :map prog-mode-map
       ;; [jalali:1403/04/18/20:45]
       ;; I added this as TAB was somehow getting mapped to [help:evil-jump-item]. I am not sure if we should bind both TAB and <tab>.
       :ing
       "TAB" #'indent-for-tab-command
       :ing
       "<tab>" #'indent-for-tab-command))

(setq doom-localleader-key "\\")
(setq doom-localleader-alt-key "M-\\")

(define-key global-map [remap swiper] nil)

(map! :leader
      ;; :desc "M-x" "SPC" #'night/fzf-M-x

      :desc "M-x" "SPC" #'execute-extended-command

      "fp" #'+ivy/projectile-find-file

      "ss"
      ;; #'swiper
      #'consult-line
      ;; @seeAlso [help:night/consult-ugrep-buffer]
      ;; Default was [help:+default/search-buffer], which used =swiper-isearch=, which has this bug:
      ;; [[https://github.com/oantolin/orderless/issues/164][{Q} How do I use `orderless-style-dispatchers` with `ivy`? · Issue #164 · oantolin/orderless]]

      "sb"
      #'consult-line-multi

      "sB"
      #'night/consult-line-all-buffers
      )
(comment (map!
  :ng "M-x" #'night/fzf-M-x ;; #'execute-extended-command
  ))
;;;
(defun night/setup-input-decode ()
  (dotimes (i 26)
    ;; This loop adds cmd + all the lower case letters.
    ;;;
    (let ((letter (char-to-string (+ i ?a)))
          (code (+ i 97)))              ; 97 is the ASCII code for 'a'
      (define-key input-decode-map (format "\e[27;33;%d~" code) (kbd (format "s-%s" letter)))))
  ;; (define-key input-decode-map "\e[27;33;105~" [?\s-i])
  ;; (define-key input-decode-map "\e[27;33;107~" [?\s-k])
  (define-key input-decode-map "\e[27;33;44~" [?\s-,])
  ;; @GPT4 The issue is with the `[s-,]` part. The correct syntax for a key sequence in Emacs Lisp is a vector of symbols or characters, not a list. You should use a vector instead of a list, =[?\s-,]=.
;;; Adding Shift modified keys:
(define-key input-decode-map "\e[27;2;32~" [?\S- ])
;;;
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

;; input-decode-map is reset for each terminal instance.
(add-hook 'tty-setup-hook #'night/setup-input-decode)
;;;

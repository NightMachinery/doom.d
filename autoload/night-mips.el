;;; night-mips.el ---                                -*- lexical-binding: t; -*-
;; https://github.com/hlissner/emacs-mips-mode
;;
;; =C-c C-c= evaluates the current buffer in =mips-interpreter=
;;
;; =C-c C-r= evaluates the current region in =mips-interpreter=
;;;

(require 'mips-mode)


(comment
 (add-to-list 'auto-mode-alist '("\\.\\(asm\\)\\'" . mips-mode)))
(add-to-list 'auto-mode-alist '("\\.\\(mips\\)\\'" . mips-mode))

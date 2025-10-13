;;; ~/doom.d/night-evil.el -*- lexical-binding: t; -*-

;; (evilem-default-keybindings "s")      ; #easymotion
  (setq evil-want-fine-undo t)

;; (evil-define-minor-mode-key 'motion 'visual-line-mode "j" 'evil-next-visual-line)
;; (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-previous-visual-line)
(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq evil-emacs-state-cursor '("chartreuse3" (bar . 2)))
(setq evil-cross-lines t)
(setq evil-move-beyond-eol t)
(setq evil-move-cursor-back nil)
;;;

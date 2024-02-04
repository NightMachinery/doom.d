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
(comment   (setq evil-snipe-override-local-mode-map
                 (let ((map (make-sparse-keymap)))
                   (evil-define-key* 'motion map
                     "f" #'evil-snipe-f
                     "F" #'evil-snipe-F
                     "t" #'evil-snipe-t
                     "T" #'evil-snipe-T)
                   (when evil-snipe-override-evil-repeat-keys
                     (evil-define-key* 'motion map
                       "n" #'evil-snipe-repeat ;; Replacing the default ';'
                       "N" #'evil-snipe-repeat-reverse))
                   map))

           (setq evil-snipe-parent-transient-map
                 (let ((map (make-sparse-keymap)))
                   (define-key map "n" #'evil-snipe-repeat)
                   (define-key map "N" #'evil-snipe-repeat-reverse)
                   map))
           )

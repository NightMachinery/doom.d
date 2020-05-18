
;; (evilem-default-keybindings e")      ; #easymotion

(setq evil-cross-lines t)
(setq evil-move-beyond-eol t)
(setq evil-move-cursor-back nil)


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

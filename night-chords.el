;;; ~/doom.d/autoload/night-chords.el -*- lexical-binding: t; -*-

(key-chord-mode 1)
(key-chord-define-global "fd" '(lambda () (interactive "")
                                 (cond
                                  ((or (eq evil-state 'normal) (eq evil-state 'visual)) (execute-kbd-macro (kbd "<escape> , h h")))
                                  ((eq evil-state 'insert) (execute-kbd-macro(kbd "<escape> , h h i"))))))

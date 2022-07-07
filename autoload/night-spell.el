;;; autoload/night-spell.el -*- lexical-binding: t; -*-

(after! (flyspell)
  (set-face-attribute 'flyspell-incorrect nil
                      :underline '(:color
                                   "#ff0000"
                                   ;; "red"
                                   :style wave) ;; This might not work in TUI.
                      :background
                      ;; "salmon"
                      "pink"
                      )
  ;;;
  (map! :leader
        "s c" #'flyspell-correct-word-before-point)
  ;;;
  )

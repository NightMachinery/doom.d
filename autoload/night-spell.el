;;; autoload/night-spell.el -*- lexical-binding: t; -*-

(require 'flyspell)
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
  (set-face-attribute 'flyspell-duplicate nil
                      :background
                      "palegreen"
                      )
  ;;;
  (map! :leader
        "s c" #'flyspell-correct-word-before-point)
  ;;;
  )

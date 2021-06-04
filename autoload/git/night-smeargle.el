;;; autoload/git/night-smeargle.el -*- lexical-binding: t; -*-

(after! smeargle
  (setq smeargle-colors
        '((older-than-1day . "ivory")
          (older-than-3day . "grey99")
          (older-than-1week . "grey95")
          (older-than-2week . "grey91")
          (older-than-1month . "grey87")
          (older-than-3month . "grey83")
          (older-than-6month . "grey79")
          (older-than-1year . "grey75")))
  (setq smeargle-age-colors
        '((0 . "ivory")
          (1 . "grey99")
          (2 . "grey95")
          (3 . "grey91")
          (4 . "grey87")
          (5 . "grey83")
          (6 . "grey79")
          (7 . "grey75")))
  )

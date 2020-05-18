;;; ~/doom.d/autoload/night-zoom.el -*- lexical-binding: t; -*-

(with-eval-after-load 'zoom
  (setq zoom-size '(0.618 . 0.618))
  (spacemacs/set-leader-keys "tg" 'zoom-mode))

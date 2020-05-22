;;; ~/doom.d/autoload/night-zoom.el -*- lexical-binding: t; -*-

(with-eval-after-load 'zoom
  (zoom-mode)
  (setq zoom-size '(0.618 . 0.618))
  (night/set-leader-keys "t g" 'zoom-mode))

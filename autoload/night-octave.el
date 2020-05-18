;;; ~/doom.d/night-octave.el -*- lexical-binding: t; -*-

(add-hook 'octave-mode-hook
            (lambda () (progn (setq octave-comment-char ?%)
                         (setq comment-start "% ")
                         (setq comment-add 0))))

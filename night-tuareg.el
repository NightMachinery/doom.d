;;; ~/doom.d/night-tuareg.el -*- lexical-binding: t; -*-

(evil-define-key 'normal tuareg-mode-map (kbd ", e b") #'tuareg-eval-buffer)
  (add-hook 'tuareg-mode-hook
            (lambda () (progn
                    (company-mode -1))))

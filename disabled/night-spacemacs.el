;;; ~/doom.d/night-spacemacs.el -*- lexical-binding: t; -*-

;;; Spacemacs specific

(add-hook 'Man-mode-hook
          (lambda () (local-set-key (kbd "q") 'Man-kill)))

  (spacemacs/set-leader-keys "qx" '(lambda () (interactive) (progn (kill-buffer) (spacemacs/frame-killer))))

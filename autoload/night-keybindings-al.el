;;; ~/doom.d/autoload/keybindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "H-C-M-e") 'insert-char)
(night/set-leader-keys "z s" 'save-some-buffers)
(night/set-leader-keys "z w" 'fixup-whitespace)

(global-set-key (kbd "C-a") 'evil-beginning-of-line)
(global-set-key (kbd "C-e") 'evil-end-of-line)

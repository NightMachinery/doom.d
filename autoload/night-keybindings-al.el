;;; ~/doom.d/autoload/keybindings.el -*- lexical-binding: t; -*-

(global-set-key (kbd "H-C-M-e") 'insert-char)
(night/set-leader-keys "z s" 'save-some-buffers)
(night/set-leader-keys "z w" 'fixup-whitespace)

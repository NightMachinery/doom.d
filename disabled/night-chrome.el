;;; ~/doom.d/night-chrome.el -*- lexical-binding: t; -*-

(require 'atomic-chrome)
(ignore-errors (atomic-chrome-start-server))

(add-hook 'edit-server-done-hook (lambda () (shell-command "open -a \"Google Chrome\"")))

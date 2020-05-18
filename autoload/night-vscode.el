;;; ~/doom.d/night-vscode.el -*- lexical-binding: t; -*-

(defun night/vscode ()
  (interactive)
  (shell-command
   (concat "code-insiders -r " (shell-quote-argument (buffer-file-name)))))

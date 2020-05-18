;;; ~/doom.d/night-file.el -*- lexical-binding: t; -*-

(defun night/make-buffer-executable ()
  (interactive)
  (shell-command
   (concat "chmod u+x " (shell-quote-argument (buffer-file-name)))))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

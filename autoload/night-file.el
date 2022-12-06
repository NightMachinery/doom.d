;;; ~/doom.d/night-file.el -*- lexical-binding: t; -*-

(defun night/make-buffer-executable ()
  (interactive)
  (shell-command
   (concat "chmod u+x " (shell-quote-argument (buffer-file-name)))))
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
;;;
(defun night/change-file-extension (&optional ext)
      (interactive)
      (let* (
             ;;; 
             ;; (new-extension (read-from-minibuffer "Type the new extension including the dot (.): "))
             ;; (new-file-name (concat (file-name-sans-extension buffer-file-name) new-extension))
             ;;;
             (new-extension (or ext
                                (ivy-read "(Also saves the current buffer!) Type the new extension (without the dot): " '("org" "md"))))
             (new-file-name (concat (file-name-sans-extension buffer-file-name) "." new-extension))
             (filename (buffer-file-name)))
        (save-buffer)
        (rename-file filename new-file-name t)
        (rename-buffer (concat (file-name-sans-extension (buffer-name)) new-extension))
        (set-visited-file-name new-file-name)
        (set-buffer-modified-p nil)
        (message (concat "File renamed to " new-file-name))))

(defun night/extension-set-to-org ()
  (interactive)
  (night/change-file-extension "org"))
;;;

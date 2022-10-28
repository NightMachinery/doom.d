;;; autoload/org/night-refile.el -*- lexical-binding: t; -*-
;;;
(defvar night/last-yank-pwd nil)
(defun night/h-yank-save-pwd (&rest args)
  (setq night/last-yank-pwd default-directory))
(advice-add #'kill-new :after #'night/h-yank-save-pwd)

(defun night/org-paste-with-files (&optional arg)
  "Pastes the content in the clipboard. Also copies any files that were linked in that content under the old PWD.

With prefix argument ARG, move the files instead."
  (interactive "P")
  (let*
      (
       (text (current-kill 0))
       (files (zf reval-withstdin (identity text) rget "\\[file:([^.](?:[^]]|\\\\[|\\\\])*)\\]" --color never))
       (stdout ""))
    (dolist (f files stdout)
      (let*
          ((f-abs (concat night/last-yank-pwd "/" f)))
        (setq stdout
              (concat
               stdout
               (z h-org-cp-file
                  (or arg "n")
                  (identity f-abs)
                  (concat default-directory "/" f))))))
    (night/insert-for-yank-and-save text)
    (message "%s" stdout)))
;;;

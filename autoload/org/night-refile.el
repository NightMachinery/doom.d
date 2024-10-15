;;; autoload/org/night-refile.el -*- lexical-binding: t; -*-
;;;
(defun night/org-paste-with-files (&optional arg)
  "Pastes the content in the clipboard. Also copies any files that were linked in that content under the old PWD.

With prefix argument ARG, move the files instead."
  (interactive "P")
  (let*
      (
       (text (current-kill 0))
       (files
        (zf
         reval-withstdin (identity text)
         rget
         "\\[file:([^/](?:[^]]|\\\\[|\\\\])*)\\]"
         ;; "\\[file:([^.](?:[^]]|\\\\[|\\\\])*)\\]"
         ;; @idk why we had excluded paths that started with a dot
         ))
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
    (night/org-insert-and-fix-levels text)
    (night/save-buffer)
    (message "%s" stdout)))
;;;

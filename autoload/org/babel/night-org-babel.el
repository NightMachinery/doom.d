;;; autoload/org/night-org-babel.el -*- lexical-binding: t; -*-

(after! (org evil-org)
  ;; (setq org-babel-min-lines-for-block-output 0)
  ;; If number of lines of output is equal to or exceeds this value, the output is placed in a #+begin_example...#+end_exampleblock.
  (setq org-babel-min-lines-for-block-output 9999999)
  ;; @upstreamBug emacs-jupyter can't handle the wrapping blocks. Use =:wrap= for other langs.
;;;
  (defun night/org-babel-execute-maybe-and-backward ()
    (interactive)
    (org-babel-execute-maybe)
    (org-babel-previous-src-block))

  (defun night/org-babel-execute-maybe-and-forward ()
    (interactive)
    (org-babel-execute-maybe)
    (org-babel-next-src-block))

  (map! :map 'evil-org-mode-map
        :localleader
        "za" #'night/org-babel-execute-maybe-and-backward
        "zx" #'night/org-babel-execute-maybe-and-forward
        "zs" #'org-babel-execute-subtree
        "zz" #'org-ctrl-c-ctrl-c)
;;;
  (comment
   (progn
     (night/unadvice 'org-babel-do-load-languages)
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (julia . t)
        (python . t)
        (C . t)
        (jupyter . t))))))

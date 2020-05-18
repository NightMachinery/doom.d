(defun night/eval-line ()
  (interactive)
  (save-mark-and-excursion
    (my-select-current-line)
    (command-execute 'eval-region)))
;;
(defun my-lisp-init ()
  (progn
    ;; No need. Use the 'o' text object ;) ;; (modify-syntax-entry ?- "w") ;; Makes word-word one word.
    ;; make evil-lispy start in the modes you want
    ;; (evil-lispy-mode)
    (lispy-mode 1)
    ;; lispyville is automatically started as well.
    ))
(lispyville-set-key-theme '(operators c-w 
;; additional
))
(comment
(lispyville--define-key '(insert)
                        "\"" #'lispy-doublequote) ;;Otherwise would escape doublequotes in Strings automagically.

( comment (lispyville--define-key '(normal visual)
                        "P" #'lispy-paste) )
(lispyville--define-key '(normal visual motion)
                        "H" #'lispyville-backward-sexp
                        "L" #'lispyville-forward-sexp
                        (kbd "M-h") #'lispyville-beginning-of-defun
                        (kbd "M-l") #'lispyville-end-of-defun
                        ;; reverse of lispy-flow
                        "{" #'lispyville-previous-opening
                        "}" #'lispyville-next-closing
                        ;; like lispy-flow
                        ;; "8" #'lispyville-next-opening
                        ;; "7" #'lispyville-previous-closing
                        ;; like lispy-left and lispy-right
                        "(" #'lispyville-backward-up-list
                        ")" #'lispyville-up-list)
(add-hook 'lispy-mode-hook #'lispyville-mode)
(add-hook 'emacs-lisp-mode-hook #'my-lisp-init)
(add-hook 'clojure-mode-hook  #'my-lisp-init)
(add-hook 'scheme-mode-hook #'my-lisp-init)
(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
)

;;; keys
;(map! :localleader "e w" #'night/eval-line)

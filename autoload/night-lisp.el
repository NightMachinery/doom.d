;; See http://oremacs.com/lispy/ for lispy's reference.
;;;
(defun night/eval-line ()
  (interactive)
  (save-mark-and-excursion
    (my-select-current-line)
    (command-execute 'eval-region)))

(defun night/backward-up-sexp (arg)
  ;; TODO doesn't work if you're on ( of sexp
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (night/backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(defun night/select-current-sexp (arg)
  (interactive "p")
  (night/backward-up-sexp arg)
  (mark-sexp)
  (setq deactivate-mark nil))

(defun night/eval-current-sexp (arg)
  (interactive "p")
  (save-mark-and-excursion
    (night/select-current-sexp arg)
    (command-execute 'eval-region)))
;;;
(defun night/h-goto-paren-move-around ()
  (let ((c (char-after)))
    (cond
     ;; ((equalp c ?\()
     ;;  nil)
     ((equalp c ?\))
      (night/point-increment))
     (t
      (message "char: %s" (char-to-string c))))))

(defun night/point-increment ()
  ;; (interactive)
  (goto-char (min (point-max) (+ (point) 1))))

(defun night/point-decrement ()
  ;; (interactive)
  (goto-char (max 1 (- (point) 1))))

(cl-defun night/h-goto-paren-gen (&key (count 1) ;; currently only supporting 1 and -1
                                       (pattern "(\\|)")
                                       (hook-after #'night/h-goto-paren-move-around))
  (let ((repeat t))
    (while repeat
      (let (
            (c (char-after (point)))
            (c1 (char-after (+ (point) 1)))
            (c_1 (char-after (- (point) 1)))
            (c_2 (char-after (- (point) 2))))
        (cond
         ((<= count -1) (cond
                         ((or
                           (equalp c_1 ?\()
                           (equalp c_2 ?\)))
                          (night/point-decrement))
                         (t
                          (night/point-decrement)
                          (search-backward-regexp pattern nil t)
                          (funcall hook-after))))
         ((>= count 1)
          (cond
           ((or (equalp c1 ?\()
                (equalp c ?\)))
            (night/point-increment))
           (t
            (night/point-increment)

            (when (search-forward-regexp pattern nil t)
              (night/point-decrement))
            (funcall hook-after))))))
      (when (not (or
                  ;; (lispy--in-comment-p)
                  (lispy--in-string-p)))
        (setq repeat nil)))))

(defun night/goto-previous-paren ()
  (interactive)
  (night/h-goto-paren-gen :count -1))

(defun night/goto-next-paren ()
  (interactive)
  (night/h-goto-paren-gen :count 1))

(defun night/goto-previous-closing-paren ()
  (interactive)
  (goto-char (max 1 (- (point) 1)))
  (lispyville-previous-closing)
  (goto-char (+ (point) 1)))

(defun night/goto-next-closing-paren ()
  (interactive)
  (goto-char (max 1 (- (point) 1)))
  (lispyville-next-closing)
  (goto-char (+ (point) 1)))
;;;
(defun my-lisp-init ()
  (interactive)
  (progn
    ;; No need. Use the 'o' text object ;) ;; (modify-syntax-entry ?- "w") ;; Makes word-word one word.
    ;; make evil-lispy start in the modes you want
    ;; (evil-lispy-mode)
    (lispy-mode 1)
    ;; lispyville is automatically started as well.
    (global-set-key (kbd "M-O") nil)
    (unbind-key "M-DEL" lispy-mode-map)
    (unbind-key "M-O" lispy-mode-map)
    (unbind-key "M-O" lispy-mode-map-evilcp) ; Probably redundant
    (unbind-key "M-O" lispy-mode-map-lispy)
    (unbind-key "M-<left>" lispy-mode-map)
    (unbind-key "M-<left>" lispy-mode-map-lispy)
    (unbind-key "M-<right>" lispy-mode-map)
    (unbind-key "M-<right>" lispy-mode-map-lispy)
    ))
;; (lispy-set-key-theme '(lispy c-digits))
(lispyville-set-key-theme '(operators c-w
                                      ;; additional
                                      ))
(eval-after-load "lispy"
  `(progn
     ;; replace a local binding
     (lispy-define-key lispy-mode-map "U" 'lispy-wrap-round)))

(progn
;;;
  (lispyville--define-key '(insert)
;;;
    ;; ;; This has the problem that the unpaired "s will then be undeletable
    ;; "\""
    ;; ;; #'lispy-doublequote
    ;; #'(lambda ()
    ;;     (interactive)
    ;;     (insert "\""))
    ;; ;;Otherwise would escape doublequotes in Strings automagically.
;;;
    ;; "[" #'night/avy-goto-opening-paren
    ;; "]" #'night/avy-goto-closing-paren
    "[" #'night/goto-previous-paren
    "]" #'night/goto-next-paren
    )
;;;

  (lispyville--define-key '(normal visual)
    "p" #'lispy-paste)

  (lispyville--define-key '(normal visual motion)
    "H" #'lispyville-backward-sexp
    "L" #'lispyville-forward-sexp
    (kbd "M-h") #'lispyville-beginning-of-defun
    (kbd "M-l") #'lispyville-end-of-defun
    ;; reverse of lispy-flow
    "{" #'lispyville-previous-closing
    "}" #'lispyville-next-closing
    ;; like lispy-flow
    ;; "8" #'lispyville-next-opening
    ;; "7" #'lispyville-previous-closing
    ;; like lispy-left and lispy-right
;;;
    "(" #'lispyville-backward-up-list
    ")" #'lispyville-up-list
    )
  (map! :map lispyville-mode-map
        ;; :n
        ;; "g s 9" #'lispy-ace-paren
        ;; :n
        ;; "g s 0" #'lispy-ace-symbol      ; bound to Q by lispy
;;;
        ;; don't work :| lispyville--define-key didn't work either
        ;; this is beacuse they have used `[remap evil-delete-char] #'lispyville-delete-char-or-splice`
        ;; :nmv
        ;; "x" #'evil-delete-char
        ;; :nmv
        ;; "X" #'evil-delete-backward-char
        )
  (add-hook 'lispy-mode-hook #'lispyville-mode)
  (add-hook 'emacs-lisp-mode-hook #'my-lisp-init)
  (add-hook 'lisp-mode-hook #'my-lisp-init)
  (add-hook 'clojure-mode-hook  #'my-lisp-init)
  (add-hook 'scheme-mode-hook #'my-lisp-init)
  (defun conditionally-enable-lispy ()
    (when (eq this-command 'eval-expression)
      (lispy-mode 1)))
  (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy))

;;; keys
(after! elisp-mode
  (map! :localleader
        :map emacs-lisp-mode-map
        "e c" #'night/eval-current-sexp
        "e l" #'night/eval-line))

;; TODO https://github.com/hchbaw/eval-sexp-fu.el/blob/master/eval-sexp-fu.el
;; `v a f` selects correctly, we just need to script it.

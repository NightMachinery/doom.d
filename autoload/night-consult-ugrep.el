;;; night-consult-ugrep.el ---                       -*- lexical-binding: t; -*-
;;;
(after! (night-consult)
  (defvar night/h-consult-ugrep-in-progress nil)

  (defvar night/consult-ugrep-args
    (string-join '("ugrep"
                   "--color=never"
                   "--exclude-dir=.git/"
                   "--hidden"
                   "--ignore-binary"
                   "--smart-case"
                   "--line-buffered"
                   "--line-number"
                   "--null"
                   "--recursive"
                   "--bool"
                   "--perl-regexp")
                 " "))
  (defun night/consult-ugrep-make-builder (paths)
    "Create ugrep command line builder given PATHS."
    (let ((cmd (consult--build-args night/consult-ugrep-args)))
      (lambda (input)
        (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                     (`(,re . ,hl)
                      (funcall consult--regexp-compiler arg 'extended t)))
          (when re
            (cons
             (append
              cmd
              (cond
               (t
                ;; (message "ugrep input: %s" input)

                ;; We give the input directly to ugrep, without any further processing.
                (list "-e" input))
               (t (cdr
                   (mapcan
                    (lambda (x)
                      (let*
                          ((x
                            (cond
                             ((s-starts-with-p "!" x)
                              (concat "-" (substring x 1))
                              ;; converting orderless's syntax to ugrep
                              )
                             (t x))))
                        (list "--and" "-e" x)))
                    re))))
              opts paths)
             hl
             ;; nil
             ))))))
  (defun night/consult-ugrep-make-builder-fast (paths)
    ;; This is no faster though?
    "Create ugrep command line builder given PATHS."
    (let ((cmd (consult--build-args night/consult-ugrep-args)))
      (lambda (input)
        ;; (message "ugrep input: %s" input)
        (cons
         (append
          cmd
          ;; We give the input directly to ugrep, without any further processing.
          (list "-e" input)
          paths)
         nil
         ))))

  (defun night/consult-ugrep (&optional dir initial prompt)
    "Search with `ugrep' for files in DIR with INITIAL input."
    (interactive)
    (let ((prompt (or prompt "ug"))
          (night/h-consult-ugrep-in-progress t))
      (consult--grep prompt
                     #'night/consult-ugrep-make-builder-fast
                     ;; #'night/consult-ugrep-make-builder
                     dir initial)))

  (defun night/consult-ugrep-buffer (&optional initial)
    (interactive)
    (night/consult-ugrep (list (buffer-file-name)) initial)))
;;;

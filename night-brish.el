;;; autoload/night-brish.el -*- lexical-binding: t; -*-

(defun night/h-brishz-arg (arg)
  (cond
   ((symbolp arg)
    (symbol-name arg))
   ((listp arg)
    (comment only called from -z-helper as night/brishz flattens its arguments)
    (eval arg)
    )
   (t
     (format "%s" arg)
)))

(defun night/brishz (&rest args)
  (interactive
   (let ((cmd (read-string "Command: " nil 'night-brishz-history)))
     (list "eval" cmd)
     )
   )
  (let* ((str-args '())
         (errbuffname "*brishz-err*")
         (errbuff (get-buffer-create errbuffname))
         (error-file (make-temp-file "brishz-err-file")
                     ))
    (dolist (arg (-flatten args))
      (push (night/h-brishz-arg arg) str-args))
    (with-output-to-string
      (apply #'call-process "brishzq.zsh" nil (list standard-output error-file) nil (nreverse str-args))


      ;; display errors, stolen from emacs' `shell-command` function, stolen from https://emacs.stackexchange.com/questions/12450/capturing-stderr-of-subprocesses?noredirect=1&lq=1
      (when (and error-file (file-exists-p error-file))
        (if (< 0 (nth 7 (file-attributes error-file)))
            (with-current-buffer errbuff
              (let ((pos-from-end (- (point-max) (point))))
                (or (bobp)

                    ;; \f skips to the start of the next page.
                    ;; (insert "\f\n")
                    (insert "\n\n=================\n=================\n\n")

                    )
                ;; Do no formatting while reading error file,
                ;; because that can run a shell command, and we
                ;; don't want that to cause an infinite recursion.
                (format-insert-file error-file nil)
                ;; Put point after the inserted errors.
                (goto-char (- (point-max) pos-from-end)))
              (+popup/buffer)
              )))
      )))

;; (defmacro z (&rest args)
;;   (-concat (list 'night/brishz ) (mapcar #'night/h-brishz-arg (-flatten (mapcar #'night/h-brishz-arg args)))))
(defmacro z (&rest args)
  (-concat (list '-z-helper (list 'quote args))))

(defun -z-helper (args)
  (night/brishz (mapcar #'night/h-brishz-arg (-flatten (mapcar #'night/h-brishz-arg args))))
  )

(defmacro zf (&rest args)
  (list 'split-string (append '(z) args) "\n" t)
  )
(defmacro z0 (&rest args)
  (list 'split-string (append '(z) args) "\0" t)
  )
(defmacro mycomment (&rest a)
t
)

(defun -z-test1 ()
  (interactive)
  (z ec (buffer-file-name)))
;;; tests:
(mycomment
 (z ecn hi)
 (z "ecn" hi)
 (z ecerr moo)
 (setq a (z ecerr moo))

 (z eval "arrN {1..20} >&2 ; echo -n Done';)'")

 (z du -h (split-string (z ls -a) "\n" t))

 (z du -h (zf ls -a))

 (z du -h (zf ls -a))
 (z rin (z arrN (z0 arr0 1 2 3)) prefixer -o ", ")

 )
;;;

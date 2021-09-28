;;; tmp.el -*- lexical-binding: t; -*-
;;;
(sly-eval `(slynk-backend:describe-symbol-for-emacs "mapcar"))
(sly-eval `(cl:documentation 'mapcar 'function) :cl)

(sly-eval `(slynk:describe-to-string (slynk:parse-symbol-or-lose 'mapcar)) :cl)

;;;
(abbreviate-file-name
 (f-expand "~/tmp"))
;;;
(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30"
  (interactive)
  (let (-sort-by -arg)
    (setq -sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal -sort-by "name") (setq -arg "-Al --si --time-style long-iso "))
     ((equal -sort-by "date") (setq -arg "-Al --si --time-style long-iso -t"))
     ((equal -sort-by "size") (setq -arg "-Al --si --time-style long-iso -S"))
     ((equal -sort-by "dir") (setq -arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other -arg )))
;;;
(native-compile-async doom-private-dir 'recursively)
;;;
(fset 'a
   (kmacro-lambda-form [?i ?\C-\[ ?\[ ?1 ?\; ?3 ?D ?\C-\[ ?\[ ?1 ?\; ?3 ?D ?\C-\[ ?\[ ?1 ?\; ?3 ?C ?\C-\[ ?\C-? escape ?/ ?: ?\C-m ?\C-\[ ?O ?C ?y ?w ?i ?\[ ?\[ ?\C-\[ ?O ?C ?\C-\[ ?\[ ?2 ?0 ?0 ?~ ?m ?o ?n ?t ?h ?  ?\C-\[ ?\[ ?2 ?0 ?1 ?~ ?\C-? ?\C-\[ ?\[ ?1 ?\; ?3 ?C ?\C-\[ ?O ?C ?\C-? ?\C-\[ ?O ?B ?\C-\[ ?O ?C escape] 1 "%d"))

;;;
(async-start (lambda () (night/brishz "bello")) (lambda (x) (message "finished: %s" x)))

(async-start
   ;; What to do in the child process
   (lambda ()
     (message "This is a test")
     (sleep-for 3)
     222)

   ;; What to do when it finishes
   (lambda (result)
     (message "Async process done, result should be 222: %s" result)))
;;;
;;

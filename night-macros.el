;;; ~/doom.d/night-macros.el -*- lexical-binding: nil; -*-

(defun night/h-defun-named (sym)
  ;; (message "sym: %s" sym)
  (cond
   ((equal sym
           '$0)
    (symbol-name name))
   ((and
     (listp sym)
     (not (equal (car-safe sym) 'quote)))
    (mapcar #'night/h-defun-named sym))
   (t
    sym)))

(defmacro night/defun-named (name &rest args)
  (declare (doc-string 3) (indent 2))
  (-concat
   (list 'defun name)
   (mapcar
    #'night/h-defun-named
    args)))

(night/defun-named night/defun-named-test ()
  (interactive)
  (insert (concat (symbol-name '$0) " $0 : My name is " $0 "!")))

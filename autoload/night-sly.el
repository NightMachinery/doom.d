;;; autoload/night-sly.el -*- lexical-binding: t; -*-

(defun night/sly-eval-str (string)
  (let ((res nil))
    (sly-eval-async `(slynk:eval-and-grab-output ,string)
      (lambda (result)
        (cl-destructuring-bind (output value) result
          (setf res (car (read-from-string value))))))
    (while (null res)
      (sleep-for 0.1))
    res))

(comment
 (night/sly-eval-str "(documentation 'mapcar 'function)"))

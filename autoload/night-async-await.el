;;; night-async-await.el ---                         -*- lexical-binding: t; -*-
;;;
(require 'async-await)

(defun night/wait-async (n)
  "Asynchronous wait."
  (promise-new (lambda (resolve _reject)
                 (run-at-time n
                              nil
                              (lambda ()
                                (funcall resolve n))))))

(defun night/brishz-async (&rest args)
  "Asynchronous brishz. (@broken this doesn't magically make sync code run async, as the code needs to cooperatively yield which sync code does not.)"
  (promise-new (lambda (resolve _reject)
                 (funcall resolve (apply #'night/brishz args)))))
;;;
(comment (async-defun example2 ()
           "As it is executed asynchronously, the process returns immediately."
           (await (night/brishz-async "fsay" "starting"))
           (await (night/brishz-async "eval" "sleep 5 && fsay slept"))
           (print (await (night/wait-async 0.5)))
           (message "---")

           (print (await (night/wait-async 1.0)))
           (message "---")
           (await (night/brishz-async "bell-lm-strawberryjuice"))

           (print (await (night/wait-async 1.5)))
           (message "---")

           (message "await done"))

         (example2))
;;;
(provide 'night-async-await)
;;; night-async-await.el ends here

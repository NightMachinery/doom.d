;;; autoload/night-metaprogramming.el -*- lexical-binding: t; -*-

(defun night/calling-function-get ()
  "BUGGY: Do NOT use"
  (or (progn ;; with-demoted-errors "Error: %S" ;; using this macro makes the logic go wrong
        (let ((n 6) ;; nestings in this function + 1 to get out of it
              func
              bt)
          (while (and (setq bt (backtrace-frame n))
                      (not func))
            (setq n (1+ n)
                  func (and bt
                            (nth 0 bt)
                            (nth 1 bt))))
          func))
      "unknown-function"
      ))

;;;

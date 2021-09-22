;;; autoload/night-eglot.el -*- lexical-binding: t; -*-

(defun night/at-point-symbol-str ()
  (interactive)
  (ignore-errors
    (substring-no-properties (thing-at-point 'symbol))))

(after! (eglot)

  (defun night/eglot-julia-documentation-str-get (&optional identifier)
    ;; [[https://github.com/joaotavora/eglot/discussions/744][joaotavora/eglot#743 {Q,FR} How do I get the documentation for an arbitrary s...]]
;;;
    ""
    (interactive)
    (let*
        ((response
          (jsonrpc-request (eglot--current-server-or-lose) :julia/getDocFromWord
                           ;; (eglot--TextDocumentPositionParams)
                           `(:word ,(or identifier
                                        (night/at-point-symbol-str))))
          ))
;;; @exfiltrate
      (setq r response)
      ;; (prin1 r)
;;;
      response ;; in markdown
      ))

  )

;;; night-detect-lang.el ---                         -*- lexical-binding: t; -*-
;;; Code:
(cl-defun night/detect-language (content &key (backend 'pygmentize))
  ;; @broken @o1
  ;; [[id:b6a25539-ddc3-4d3c-9c7a-7443a400bd24][Programming Language Detector]]
  ;; I cannot even get a PL detector to work on the CLI.
;;;
  "Detect the programming language of CONTENT using BACKEND and return the file extension."
  (let ((lang
         (pcase backend
           ('pygmentize
            ;; Use pygmentize for language detection
            (with-temp-buffer
              (insert content)
              ;; BUG should pass filename to pygmentize
              (shell-command-to-string "pygmentize -N -f token -l guess")))
           ('linguist
            ;; Use linguist for language detection
            (let ((temp-file (make-temp-file "linguist-")))
              (with-temp-file temp-file
                (insert content))
              (let ((output (shell-command-to-string
                             (format "linguist %s --json" temp-file))))
                (delete-file temp-file)
                (let* ((json-object-type 'hash-table)
                       (json-array-type 'list)
                       (json (json-read-from-string output))
                       (languages (gethash "languages" json)))
                  ;; Return the most probable language
                  (caar languages)))))
           (_ (error "Unsupported backend: %s" backend)))))
    ))

(provide 'night-detect-lang)
;;; night-detect-lang.el ends here

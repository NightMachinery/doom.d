;;; autoload/night-dumb-jump.el -*- lexical-binding: t; -*-

(after! (dumb-jump)
  (setq dumb-jump-force-searcher 'rg)
;;;
  ;; @alt We can override `dumb-jump-get-language' to make .org files return `python` as their language, but the current solution is more robust, I think.
  (defun night/h-dumb-jump-add-python-rules-to-org ()
    "Add Python rules to Org mode in Dumb Jump."
    (interactive)
    (let ((python-rules (cl-remove-if-not (lambda (rule)
                                            (string= (plist-get rule :language) "python"))
                                          dumb-jump-find-rules)))
      (setq dumb-jump-find-rules
            (append dumb-jump-find-rules
                    (mapcar (lambda (rule)
                              (plist-put (copy-tree rule) :language "org"))
                            python-rules)))))
  (night/h-dumb-jump-add-python-rules-to-org)
;;;
  )

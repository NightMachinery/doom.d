;;; autoload/org/links/night-org-links-sioyek.el -*- lexical-binding: t; -*-

(defun night/org-link-sioyek-follow (link arg)
  (cl-destructuring-bind (path &optional args)
      (split-string link "::")
    (cl-destructuring-bind (&optional page-number offset-x offset-y zoom-level)
        (cond
         ((length> args 0) (split-string args ":"))
         (t nil))
      (let* (
             (cmd (list
                   "open-sioyek-v2"
                   ;; "sioyek"
                   path)))
        (when page-number
          (setq cmd (append
                     cmd
                     (list "--page"
                           page-number
                           ;; (+ 1 (string-to-number page-number))
                           ;; fixed in [agfi:h-sioyek-org-pdf-link-create] instead
                           ))))
        (when offset-x
          (setq cmd (append
                     cmd
                     (list "--xloc" offset-x))))
        (when offset-y
          (setq cmd (append
                     cmd
                     (list "--yloc" offset-y))))
        (when zoom-level
          (setq cmd (append
                     cmd
                     (list "--zoom" zoom-level))))
        (night/brishz-ll
         :callback t
         :command cmd)))))

(org-link-set-parameters "sioyek-v1" :follow #'night/org-link-sioyek-follow)

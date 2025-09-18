;;; night-org-inline-copy.el --- Copy inline =...= / ~...~ on DWIM, skip original -*- lexical-binding: t; -*-

(with-eval-after-load 'org
  (require 'org-element)

  (defgroup night/org-inline-copy nil
    "Copy inline Org contents when triggering DWIM."
    :group 'org)

  (defcustom night/org-inline-copy-types '(verbatim code)
    "Inline Org object types that trigger copying."
    :type '(repeat (choice (const verbatim) (const code))))

  (defun night/h-dwim-copy-a (orig-fn &rest args)
    (let ((verbosity-level 0))
      (cond
       ((derived-mode-p 'org-mode)
        (condition-case err
            (let* ((ctx (org-element-context))
                   (typ (and ctx (org-element-type ctx))))
              (cond
               ;; If the inline type matches, copy its value (if any) and skip the original.
               ((memq typ night/org-inline-copy-types)
                (let ((val (org-element-property :value ctx)))
                  (cond (val
                         (kill-new val)
                         ;; flash inside the element
                         (let* ((b (org-element-property :begin ctx))
                                (e (org-element-property :end ctx))
                                (pb (or (org-element-property :post-blank ctx) 0))
                                (ib (1+ b)) ; skip opening = / ~
                                (ie (- e pb 1))) ; skip closing = / ~
                           (cond ((< ib ie)
                                  (night/flash-region
                                   ib ie
                                   ;; :backend 'nav-flash
                                   :face 'nav-flash-face))))
                         ;; (message "Copied: %s" val)
                         )))
                nil)
               ;; Otherwise, fall back to the original DWIM.
               (t
                (apply orig-fn args))))
          (error
           (message "night/org-inline-copy advice error: %s" (error-message-string err))
           (apply orig-fn args))))
       (t
        (apply orig-fn args)))))

  (when (fboundp '+org/dwim-at-point)
    (advice-add '+org/dwim-at-point :around #'night/h-dwim-copy-a)))

(provide 'night-org-inline-copy)
;;; night-org-inline-copy.el ends here

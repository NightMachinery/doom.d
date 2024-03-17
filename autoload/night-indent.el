;;; autoload/night-indent.el -*- lexical-binding: t; -*-

(after! (evil org evil-org)
  (let ((indent-n 4))
    (setq-default tab-width indent-n)
    (comment
     (default-value 'tab-width))
    (setq-default evil-shift-width indent-n)
    (setq-default indent-tabs-mode nil)
    ;; (setq tab-stop-list (number-sequence indent-n 200 indent-n))
    ;; (setq c-basic-offset indent-n)
    )

;;;
  (setq org-src-tab-acts-natively t)

  (defun night/h-org-mode-tab-width-advice (original-function &rest args)
    "Around advice function to temporarily set `tab-width` in org-mode."
    (if (eq major-mode 'org-mode)
        (let ((old-tab-width tab-width))
          ;; (message "old-tab-width: %s" old-tab-width)
          (setq tab-width 4)
          ;; (z fsay iced)
          (unwind-protect
              (progn
                (apply original-function args)
                ;; (z fsay monkey))
                ;; (z fsay fire)
                (setq tab-width old-tab-width))))
      (apply original-function args)))

    (dolist (fn
             (list
              ;; These didn't work, as these functions returned before actually doing their work.
              ;; But I am now using `evil-shift-width' instead, which works.
              ;; #'evil-shift-right
              ;; #'evil-shift-left
              
              ;; #'indent-for-tab-command
              ))
      (advice-add fn :around #'night/h-org-mode-tab-width-advice))
;;;
    )

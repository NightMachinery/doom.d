;;; ~/doom.d/night-man.el -*- lexical-binding: t; -*-

(setq manquery "^[0-9]+\\s-+")
  (when nil (add-hook 'Man-mode-hook
                      (lambda () (progn
                              (defun helm-swoop-pre-input-optimize ($query)
                                $query) ;; disable swoop escaping
                              ;; (spacemacs/toggle-maximize-buffer) ;; Breaks helm-man-woman
                              (setq helm-swoop-pre-input-function
                                    (lambda () (progn
                                            manquery)))))))
  ;; (defun helm-swoop-pre-input-optimize ($query)
  ;;   (when $query
  ;;     (let (($regexp (list '("\+" . "\\+")
  ;;                          '("\*" . "\\\\*")
  ;;                          '("\#" . "\\#"))))
  ;;       (mapc (lambda ($r)
  ;;               (setq $query (replace-regexp-in-string (car $r) (cdr $r) $query)))
  ;;             $regexp)
  ;;       $query)))

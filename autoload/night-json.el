;;; autoload/night-json.el -*- lexical-binding: t; -*-

(after! json-mode
  (defvar json-imenu-pattern
    '((
       nil
       "^[[:space:]\n,]*\"\\([^\"]*\\)\"[[:space:]\n]*:.*$"
       1))
    "Imenu pattern for JSON keys.")

  (defun night/imenu-json-setup ()
    (interactive)
    "Set up imenu for JSON."
    (setq-local imenu-generic-expression json-imenu-pattern)
    (setq-local imenu-create-index-function #'imenu-default-create-index-function)
    ;; [help:imenu-create-index-function]
    ;; [help:js--imenu-create-index]
    )

  (add-hook 'json-mode-hook 'night/imenu-json-setup)
;;;
  )

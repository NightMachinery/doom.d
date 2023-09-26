;;; autoload/org/babel/night-jupyter.el -*- lexical-binding: t; -*-

(require 'jupyter)

(after! (jupyter)
  (setq jupyter-long-timeout 20))
(after! (ob-async)
  (setq ob-async-no-async-languages-alist
        '("jupyter-python" "jupyter-julia")))

(after! (jupyter-org-client)
;;;
(setq-default org-babel-default-header-args:jupyter-python nil)
(add-to-list 'org-babel-default-header-args:jupyter-python '(:results . "scalar"))
;;;
  (defun night/jupyter-org-eval-line-or-region ()
    (interactive)
    (jupyter-org-with-src-block-client
     (call-interactively #'jupyter-eval-line-or-region)))

  (defun night/jupyter-org-eval-defun ()
    (interactive)
    (jupyter-org-with-src-block-client
     (call-interactively #'jupyter-eval-defun)))

  (defun night/jupyter-org-inspect-at-point ()
    (interactive)
    (jupyter-org-with-src-block-client
     (call-interactively #'jupyter-inspect-at-point)))

  (map! :map 'jupyter-org-interaction-mode-map
        :localleader
        "ee" #'night/jupyter-org-eval-line-or-region
        "ed" #'night/jupyter-org-eval-defun

        ;; "i" #'night/jupyter-org-inspect-at-point
        ;; "i" conflicts with [help:org-toggle-item]

        "ki" #'jupyter-org-interrupt-kernel
        "kr" #'jupyter-repl-restart-kernel

        "h" #'jupyter-org-hydra/body
        ))

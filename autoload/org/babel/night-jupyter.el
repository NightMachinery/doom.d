;;; autoload/org/babel/night-jupyter.el -*- lexical-binding: t; -*-

(require 'jupyter)

(after! (ob-async)
  (setq ob-async-no-async-languages-alist
        '("jupyter-python" "jupyter-julia")))

(after! (jupyter-org-client)
  (map! :map 'jupyter-org-interaction-mode-map
        :localleader
        "ee" #'jupyter-eval-line-or-region
        "ed" #'jupyter-eval-defun

        "i" #'jupyter-inspect-at-point

        "ki" #'jupyter-org-interrupt-kernel
        "kr" #'jupyter-repl-restart-kernel

        "h" #'jupyter-org-hydra/body
        ))

;;; ~/doom.d/night-python.el -*- lexical-binding: t; -*-

(defun my/python-mode-hook ()
  ;; Somehow disabling company doesn't work. sth else probably reenables it ...
  ;; (with-eval-after-load 'company (company-mode -1))
  ;; (company-mode -1)
  ;; (with-eval-after-load 'company (add-to-list 'company-backends 'company-jedi))
  )

(elpy-enable)
;; (require 'dap-python)
(setenv "WORKON_HOME" "~/anaconda/envs")
(pyvenv-activate  "~/anaconda/bin")   ; possibly bad?
(pyvenv-mode 1)                       ;; Possibly useful?
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(add-hook 'python-mode-hook 'my/python-mode-hook)
(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"  python-shell-interpreter-args "--simple-prompt -i" python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "ipython")
  )
(defun night/pip-install-me ()
  (interactive)
  (shell-command
   (concat "pip install " (shell-quote-argument (elpy-project-root))))
  )

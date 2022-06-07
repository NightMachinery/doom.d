;;; autoload/night-python.el -*- lexical-binding: t; -*-

(remove-hook 'python-mode-hook #'poetry-tracking-mode)
(remove-hook 'python-mode-hook #'doom-modeline-env-setup-python)
(remove-hook 'python-mode-hook #'doom--enable-+web-django-mode-in-python-mode-h)

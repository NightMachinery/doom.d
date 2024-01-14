;;;
(setq +lua-lsp-dir (concat (getenv "codedir") "/Misc/lua-language-server"))
;;;
(after! (lua-mode)
  (setq lua-indent-level 3)
  (defun night/lua-startup-hook ()
    (interactive)
    (setq lua-indent-level 4))
  (add-hook 'lua-mode-hook #'night/lua-startup-hook)
  )
;;;

;;; ~/doom.d/autoload/night-julia.el -*- lexical-binding: t; -*-

(setq lsp-julia-package-dir nil)

(after! lsp-julia
  (setq lsp-julia-default-environment "~/.julia/environments/v1.4"))

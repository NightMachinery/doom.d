;;; autoload/night-cc.el -*- lexical-binding: t; -*-

(after! (eglot)
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))
  (set-eglot-client! 'c++-mode '("clangd" "-j=3" "--clang-tidy"))
  )

(after! (flymake)
;;;
  (setq-default flycheck-clang-language-standard "gnu++2a")
;;;
  (when (and
         (not (fboundp 'flymake--diag-buffer))
         (fboundp 'flymake--diag-locus))
    (defalias 'flymake--diag-buffer 'flymake--diag-locus))
  ;; https://github.com/hlissner/doom-emacs/pull/5645
  )

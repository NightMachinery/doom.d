;;; autoload/night-latex.el -*- lexical-binding: t; -*-

(defun night/latex-to-pdf-logs ()
  (interactive)
  (find-file (expand-file-name "~/logs/pdflatex_emacs.ansilog"))
  ;; @duplicateCode/f96ce923c4daa8a299a2a376062acc30
  )
(after! (tex-mode dumb-jump)
  (setq tex-indent-arg 2)

  (defun night/latex-definition-handler (_identifier)
    "Lookup handler for LaTeX definitions."
    (night/latex-jump-to-file 0))

  (set-lookup-handlers! '(latex-mode LaTeX-mode plain-TeX-mode TeX-mode)
    :definition #'night/latex-definition-handler)

  (comment
   ;; This did not work, but I just removed the dictionary backend globally in [[DOOMDIR:autoload/night-lookup.el]], which worked.
   (set-lookup-handlers!
     'latex-mode
     'TeX-latex-mode
     :definition
     #'dumb-jump-go
     ;; nil
     :xref-backend #'dumb-jump-xref-activate)))

;;; autoload/org/night-inline-pdf.el -*- lexical-binding: t; -*-
;;;
(after! org-inline-pdf
  (setq org-inline-pdf-make-preview-program "magick")
  (setq org-inline-pdf-preview-format "png"))
;;;

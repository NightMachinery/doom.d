;;; autoload/org/links/night-agfi.el -*- lexical-binding: t; -*-

(after! (org ol)
  (defun night/org-link-agfi-follow (path arg)
    (z awaysh agfi-ni (i path)))
  (org-link-set-parameters "agfi" :follow #'night/org-link-agfi-follow)

  (defface night/org-link-face-agfi '((t (:foreground "blue4"
                                          ;; :background "azure"
                                          :weight bold))) "face for nightNotes links")
  (org-link-set-parameters "agfi" :face 'night/org-link-face-agfi)

  )

;;; ~/doom.d/autoload/nnight-comics.el -*- lexical-binding: t; -*-

;; This opens the files, but only as a directory of images. It's useless.
(add-to-list 'auto-mode-alist '("\\.\\(cbr\\)\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.\\(cbz\\)\\'" . archive-mode))

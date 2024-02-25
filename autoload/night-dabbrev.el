;;; autoload/night-dabbrev.el -*- lexical-binding: t; -*-

(after! (cape)
  (require 'dabbrev)
  (setq cape-dabbrev-min-length 0
        dabbrev-case-fold-search t
        dabbrev-upcase-means-case-search t
        dabbrev-check-other-buffers t
        dabbrev-check-all-buffers t)
)

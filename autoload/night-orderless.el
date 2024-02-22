;;; autoload/night-orderless.el -*- lexical-binding: t; -*-

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))
;;;
(provide 'night-orderless)

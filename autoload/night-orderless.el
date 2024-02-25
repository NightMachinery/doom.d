;;; autoload/night-orderless.el -*- lexical-binding: t; -*-
;;;
;; @tosee [[https://github.com/minad/consult/wiki#minads-orderless-configuration]]
;;;
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides
      '(
        (file
         (orderless basic)
         ;; (styles basic partial-completion)
         )
        (consult-grep (basic))))
;;;
;; [[file:night-ivy.el::(defvar *orderless-no-fuzzy*]]
;;;
(provide 'night-orderless)

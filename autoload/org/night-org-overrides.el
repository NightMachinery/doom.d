;;; autoload/org/night-org-overrides.el -*- lexical-binding: t; -*-

(after! org-fold-core
  (defun org-fold-core-get-folding-spec-from-alias (spec-or-alias)
    ;; @PR/ready @regressionFix
  "Return the folding spec symbol for SPEC-OR-ALIAS.
Return nil when there is no matching folding spec."
  (when spec-or-alias
    (unless org-fold-core--spec-symbols
      (dolist (spec (org-fold-core-folding-spec-list))
        (push (cons spec spec) org-fold-core--spec-symbols)
        (dolist (alias (assq :alias (assq spec org-fold-core--specs)))
          (push (cons alias spec) org-fold-core--spec-symbols))))
    (or                                 ;; @monkeyPatched
     (alist-get spec-or-alias org-fold-core--spec-symbols)
     spec-or-alias))))

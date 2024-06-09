;;; autoload/night-code-cells-v1.el -*- lexical-binding: t; -*-

(with-eval-after-load 'code-cells
  (comment
   ;; the default is https://github.com/mwouts/jupytext, I think
   (setq code-cells-convert-ipynb-style '(("pandoc" "--to" "ipynb" "--from" "org")
                                          ("pandoc" "--to" "org" "--from" "ipynb")
                                          org-mode)))
  ;;;
  (let ((map code-cells-mode-map))
    (define-key map (kbd "M-p") 'code-cells-backward-cell)
    (define-key map (kbd "M-n") 'code-cells-forward-cell)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)

    ;; Overriding other minor mode bindings requires some insistence...
    ;; (define-key map [remap jupyter-eval-line-or-region] 'code-cells-eval)
;;; @speedKeys
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB")
      (code-cells-speed-key
       (lambda ()
         "Show/hide current cell"
         (interactive)
         (outline-minor-mode)
         (if (outline-invisible-p (line-end-position))
             (outline-show-subtree)
           (outline-hide-subtree)))))))

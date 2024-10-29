;;; autoload/org/night-latex.el -*- lexical-binding: t; -*-

(after! org
  ;; You can adapt the old code at http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/ to automatically change the previews to code and vice versa when the cursor enters/leaves them.
  ;; Update: we already have automatic previews ...
;;;
  (setq org-startup-with-latex-preview
        ;; t
        ;; [[id:86053cea-abb2-43fb-b1d9-8bec1b93286c][elisp: hook: check if buffer has been opened interactively]]
        nil
        )
  (setq org-preview-latex-default-process 'dvisvgm)

  ;; https://emacs.stackexchange.com/questions/19880/font-size-control-of-latex-previews-in-org-files
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))
  (setq org-format-latex-options (plist-put org-format-latex-options :foreground "black"))

  (defun night/latex-syntax-highlighting-enable ()
    (interactive)
    (setq org-latex-listings 'minted)

    (setq org-latex-custom-lang-environments
          '(
            (emacs-lisp "common-lispcode")))

    (setq org-latex-minted-options
          '(("frame" "lines")
            ("fontsize" "\\scriptsize")
            ("linenos" "")))

    (setq org-latex-to-pdf-process
;;;
          ;; '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          ;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          ;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
;;;
          ;; '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
;;;
          ;; I tried using an elisp function here, but it errored. The docs seem to be wrong here.

          ;; '("brishzq.zsh h-pdflatex-emacs %F")
          '("brishzq.zsh h-pdflatex-emacs-async %F")
;;;
          )
    ;; `org-latex-to-pdf-process' was probably deprecated.
    (setq org-latex-pdf-process org-latex-to-pdf-process)
    ;; -shell-escape  Enable the \write18{command} construct. The command can be any shell  command. This construct is normally disallowed for security reasons.
    )
  (night/latex-syntax-highlighting-enable)
  )

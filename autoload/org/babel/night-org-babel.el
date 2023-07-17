;;; autoload/org/night-org-babel.el -*- lexical-binding: t; -*-

(after! (org evil-org)
  ;; (setq org-babel-min-lines-for-block-output 0)
  ;; If number of lines of output is equal to or exceeds this value, the output is placed in a #+begin_example...#+end_exampleblock.
  (setq org-babel-min-lines-for-block-output 9999999)
  ;; @upstreamBug emacs-jupyter can't handle the wrapping blocks. Use =:wrap= for other langs.
;;;
  (defun night/org-babel-execute-maybe-and-backward ()
    (interactive)
    (org-babel-execute-maybe)
    (org-babel-previous-src-block))

  (defun night/org-babel-execute-maybe-and-forward ()
    (interactive)
    (org-babel-execute-maybe)
    (org-babel-next-src-block))
;;;
  (defun night/org-babel-select-src-and-results ()
  "Select the current org-babel cell at point along with its RESULTS block."
  (interactive)
  ;; Check if we're at a src block
  (unless (org-in-src-block-p)
    (search-backward "#+begin_src" nil t)
    ;; (+nav-flash/blink-cursor)
    ;; (sit-for 0.5)
    )
  ;; Recheck if we're at a src block
  (if (org-in-src-block-p)
      (let ((src-start (org-babel-where-is-src-block-head))
            (src-end (org-babel-where-is-src-block-result))
            results-end)
        ;; Extend the selection to include the RESULTS block if it exists
        (save-excursion
          (goto-char src-end)
          (if (re-search-forward "^#\\+RESULTS:" nil t)
              (setq results-end (org-babel-result-end))
            ;; No RESULTS block, so just select the source block
            (setq results-end src-end)))
        ;; Select the region
        (goto-char src-start)
        (push-mark results-end t t))
    (message "Not at a src block!")))
;;;

  (map! :map 'evil-org-mode-map
        :localleader
        "za" #'night/org-babel-execute-maybe-and-backward
        "zx" #'night/org-babel-execute-maybe-and-forward
        "zs" #'org-babel-execute-subtree
        "zz" #'org-ctrl-c-ctrl-c
        "zv" #'night/org-babel-select-src-and-results
        )
;;;
  (comment
   (progn
     (night/unadvice 'org-babel-do-load-languages)
     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (julia . t)
        (python . t)
        (C . t)
        (jupyter . t))))))

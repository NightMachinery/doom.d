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
  (cl-defun night/h-org-babel-execute-with-move-fn
      (&key
       (move-fn (lambda () nil))
       (no-blocks-message "No source blocks found."))
    "Execute org-mode source blocks in a specified direction using MOVE-FN.
MOVE-FN should be a function that moves to the next or previous source block.
NO-BLOCKS-MESSAGE is a message string to display if no blocks are found in the specified direction."
    (cl-block 'blk
      (save-excursion
        ;; If not inside a source block, attempt to find the next or previous one.
        (when (not (org-in-src-block-p))
          (unless (funcall move-fn)
            (message no-blocks-message)
            (cl-return-from blk)))

        ;; Begin execution of source blocks.
        (while (org-in-src-block-p)
          (org-babel-execute-src-block)
          (unless (funcall move-fn)
            (cl-return-from blk)))
        (message "Execution of source blocks completed."))))

  (defun night/org-babel-execute-after ()
    "Execute the current org-mode source block and all following ones."
    (interactive)
    (night/h-org-babel-execute-with-move-fn
     :move-fn 'org-babel-next-src-block
     :no-blocks-message "No source blocks found ahead."))

  (defun night/org-babel-execute-before ()
    "Execute the current org-mode source block and all preceding ones."
    (interactive)
    (night/h-org-babel-execute-with-move-fn
     :move-fn 'org-babel-previous-src-block
     :no-blocks-message "No source blocks found before."))
;;;
  (defun night/org-babel-select-src-and-results-v0 ()
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

  (defun night/org-babel-where-is-src-block-end ()
    (interactive)
    "Return the end position of the org babel src block at point."
    (let ((element (org-element-at-point)))
      (when (eq (car element) 'src-block)
        (org-element-property :end element))))

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
              (src-end (night/org-babel-where-is-src-block-end))
              (results-start (org-babel-where-is-src-block-result))
              results-end)
          ;; Extend the selection to include the RESULTS block if it exists
          (if results-start
              (save-excursion
                (goto-char results-start)
                (if (re-search-forward "^#\\+RESULTS:" nil t)
                    (setq results-end (org-babel-result-end))
                  ;; No RESULTS block, so just select the source block
                  (setq results-end src-end)))
            (setq results-end src-end))

          ;; Select the region
          (goto-char src-start)
          (push-mark results-end t t))
      (message "Not at a src block!")))
;;;
  (defun night/org-babel-remove-all-results ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^[ \t]*#\\+BEGIN_SRC" nil t)
        (let ((el (org-element-context)))
          (when (org-in-src-block-p)
            (org-babel-remove-result))))))
;;;

  (map! :map 'evil-org-mode-map
        :localleader
        "za" #'night/org-babel-execute-maybe-and-backward
        "zx" #'night/org-babel-execute-maybe-and-forward
        "zs" #'org-babel-execute-subtree
        "zb" #'org-babel-execute-buffer
        "zn" #'night/org-babel-execute-after
        "zp" #'night/org-babel-execute-before
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

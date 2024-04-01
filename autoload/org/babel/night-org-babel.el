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
(defun night/org-babel-remove-results-from-region (start end)
  "Remove Babel results from the specified region."

  (interactive "r") ; When called interactively, use the current region
  (save-excursion
    (goto-char start)
    (let ((end-marker (copy-marker end))) ; Use a marker to track the dynamic end position
      (while (and (< (point) end-marker)
                  (search-forward-regexp "^[ \t]*#\\+BEGIN_SRC" end-marker t))
        (let ((el (org-element-context)))
          (when (org-in-src-block-p)
            (org-babel-remove-result)))))))
(defun night/org-babel-remove-all-results ()
  "Remove all Babel results in the current buffer."
  (interactive)
  (night/org-babel-remove-results-from-region (point-min) (point-max)))

  (comment (defun night/org-babel-remove-all-results ()
     (interactive)
     (save-excursion
       (goto-char (point-min))
       (while (search-forward-regexp "^[ \t]*#\\+BEGIN_SRC" nil t)
         (let ((el (org-element-context)))
           (when (org-in-src-block-p)
             (org-babel-remove-result)))))))
;;;
(defun night/org-babel-result-get ()
  (interactive)
  "Return the result of the current source block as a string.
Assumes that the source block has already been executed."
  (interactive)
  (save-excursion
    (let ((result-beg (org-babel-where-is-src-block-result))
          result-end)
      (unless result-beg

        (error "No result found for the current source block"))
      (goto-char result-beg)
      (setq result-end (org-babel-result-end))
      (let* ((raw-result (buffer-substring-no-properties result-beg result-end))
             (result (string-trim raw-result))
             (lines (split-string result "\n"))
             (first-relevant-line-index
              (cl-position-if-not

               (lambda (line)
                 (string-match-p "^\\(#\\+\\|:RESULTS:[ \t\n]*\\|: /❂\\\)" line))
               lines))
             (last-relevant-line-index
              (cl-position-if
               (lambda (line)
                 (not
                  (string-match-p "^\\(: ----+\\|:END:\\)[ \t]*$" line)))
               lines :from-end t))
             (result
              (mapconcat
               'identity
               (cl-subseq
                lines
                (or first-relevant-line-index 0)
                (cond
                 (last-relevant-line-index
                  (+ 1 last-relevant-line-index))
                 (t
                  (+ 0 (length lines)))))
               "\n"))
             (result (replace-regexp-in-string "^\\(: \\|:$\\)" "" result t t))

             (result (string-trim result)))
        ;; (message "first: %s, last: %s, lines: %s" first-relevant-line-index last-relevant-line-index lines)
        (when (called-interactively-p)
          (kill-new result)
          (message "%s" result))
        result))))

(defun night/org-babel-copy-as-chat ()
  "Copies the result section of the current source block as the last message in an LLM chat.

@seeAlso [[file:~/code/python/PyNight/pynight/common_openai.py::def chatml_response_text_process]]
"
  (interactive)
  (let* (
         (last-msg (night/org-babel-result-get))
         (assistant
          (concat
           "        {\"role\": \"assistant\", \"content\": r\"\"\""
           last-msg
           "\"\"\"},"))
         (chat
          (concat
           assistant
           "\n        {\"role\": \"user\", \"content\": r\"\"\"\n        \n        \"\"\"},")))
    (kill-new chat)))
;;;
(cl-defun night/h-org-babel-navigate-src-block (&key (direction 'backward) (k 1))
  "Move to the next or previous Org-mode source block depending on DIRECTION.
Move the point K lines in the specified direction before starting the search.
We move between the start and beginning of blocks. `org-babel-next-src-block'/`org-babel-previous-src-block' always moves to the beginning of blocks."
  (let ((case-fold-search t) ;; Ensure case-insensitive search
        (search-fn (if (eq direction 'backward)
                       're-search-backward
                     're-search-forward))

        (block-found nil))

    ;; Use save-excursion to avoid moving the point if no block is found
    (save-excursion
      ;; Move the point k lines in the specified direction
      (if (eq direction 'backward)

          (forward-line (- k))
        (forward-line k))

      ;; Search for the source block
      (setq block-found
            (funcall search-fn
                     "^\\s-*#\\+\\(begin\\|end\\)_"
                     ;; \\(src\\|example\\|quote\\|verse\\)
                     ;; We use this for navigation, and moving to blocks of all types is actually better for us.
                     nil t)))

    ;; If a block is found, move the point to the block and center the screen
    (if block-found
        (progn
          (goto-char (match-beginning 0))
          (night/screen-center))
      (error "No %s source block found" (symbol-name direction)))))

(defun night/org-babel-previous-src-block ()
  "Move to the previous Org-mode source block.
See `night/h-org-babel-navigate-src-block'."
  (interactive)
  (night/h-org-babel-navigate-src-block :direction 'backward))

(defun night/org-babel-next-src-block ()
  "Move to the next Org-mode source block.
See `night/h-org-babel-navigate-src-block'."
  (interactive)
  (night/h-org-babel-navigate-src-block :direction 'forward))
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
        "zc" #'night/org-babel-copy-as-chat
        "zl" #'night/org-blacken-region
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

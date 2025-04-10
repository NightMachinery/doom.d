;;; autoload/org/night-org-babel.el -*- lexical-binding: t; -*-

(after! (org evil-org)
  ;;;
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

    (interactive "r")        ; When called interactively, use the current region
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
  (defalias 'night/org-babel-rm-all-results #'night/org-babel-remove-all-results)

  (comment (defun night/org-babel-remove-all-results ()
             (interactive)
             (save-excursion
               (goto-char (point-min))
               (while (search-forward-regexp "^[ \t]*#\\+BEGIN_SRC" nil t)
                 (let ((el (org-element-context)))
                   (when (org-in-src-block-p)
                     (org-babel-remove-result)))))))
;;;
  (defun night/org-babel-result-get (&optional retry-attempted)
    "Return the result of the current source block as a string.
If no result is found for the current block, try the previous
source block once. Assumes that the relevant source block has
already been executed.

When called interactively, copies the result to the kill ring
and displays it in the echo area.

If RETRY-ATTEMPTED is non-nil, do not attempt to look at the
previous block again."
    (interactive)
    (save-excursion
      (let (result-string               ; Variable to hold the final string
            (result-beg (org-babel-where-is-src-block-result)))

        (setq result-string
              (cond
               ;; --- Case 1: Result found at current location ---
               (result-beg
                (let (result-end)
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
                             ;; Match :END: or : ----... lines
                             (string-match-p "^\\(:END:\\|: [-]+[ \t]*$\\)" line))
                           lines :from-end t))
                         (cleaned-result
                          (mapconcat
                           'identity
                           (cl-subseq
                            lines
                            (or first-relevant-line-index 0)
                            ;; Use last-relevant-line-index directly if found, otherwise length
                            (or last-relevant-line-index (length lines)))
                           "\n"))
                         ;; Remove leading ": " or trailing ":" common in :value results
                         (cleaned-result (replace-regexp-in-string "^\\(: \\|:$\\)" "" cleaned-result t t))
                         (cleaned-result (string-trim cleaned-result)))
                    ;; Return the cleaned result for this cond clause
                    cleaned-result)))

               ;; --- Case 2: No result found AND already retried ---
               (retry-attempted ; This clause is only reached if result-beg is nil
                (error "No result found!"))

               ;; --- Case 3: No result found AND this is the first attempt ---
               (t ; This is the default case, reached if result-beg is nil and retry-attempted is nil
                (message "No result found for current block, trying previous...")
                ;; Move point (within save-excursion)
                (night/org-babel-previous-src-block)
                ;; Note that `night/org-babel-previous-src-block' goes to the start of the current block if already in one. So it's safe to call.
                ;; Recursively call, passing t. The return value of the recursive
                ;; call becomes the value of this cond clause.
                (night/org-babel-result-get t))))

        ;; Now result-string holds the final cleaned result (or an error occurred)
        (when (and result-string (called-interactively-p 'any))
          (kill-new result-string)
          (message "%s" result-string))
        result-string)))

  (defun night/py-escape-triple-quotes-elisp (input)
    "Replace three consecutive single or double quotes with a backslash in INPUT, unless they are already escaped.

@seeAlso [agfi:py-escape-triple-quotes]"
    (let ((pattern "\\(?:[^\\\\]\\|^\\)\\(\\(?:\"\\|'\\)\\{3\\}\\)"))
      (replace-regexp-in-string pattern "\\\\\\1" input t nil)))
  (comment
   (night/py-escape-triple-quotes-elisp "Water \"\"\"cube\\\"\"\"."))

(defun night/org-babel-result-to-chat-string ()
  "Generates a chat message string from the result section of the current source block.
Formats the result as an 'assistant' message followed by a placeholder 'user' message,
suitable for inclusion in an LLM chat context (e.g., Python list of dicts).

@seeAlso [[file:~/code/python/PyNight/pynight/common_openai.py::def chatml_response_text_process]]"
  (let* (
         (last-msg (night/org-babel-result-get))
         (last-msg (night/py-escape-triple-quotes-elisp last-msg))
         (last-msg
          (cond
           ((s-starts-with-p "\"" last-msg)
            (concat " " last-msg))
           (t last-msg)))
         (last-msg
          (cond
           ((s-ends-with-p "\"" last-msg)
            (concat last-msg " "))
           (t last-msg)))
         (assistant
          (concat
           "        {\"role\": \"assistant\", \"content\": r\"\"\""
           last-msg
           "\"\"\"},"))
         (chat
          (concat
           assistant
           "\n        {\"role\": \"user\", \"content\": r\"\"\"\n"
           "\n        \"\"\"},")))
    chat))

(defun night/org-babel-copy-as-chat ()
  "Copies the result section of the current source block as the last message in an LLM chat.
Generates the chat string using `night/org-babel-result-to-chat-string` and copies it to the kill ring."
  (interactive)
  (let ((chat-string (night/org-babel-result-to-chat-string)))
    (kill-new chat-string)
    (message "Org Babel result formatted as chat and copied.")))

(defun night/org-babel-insert-as-chat-v0 ()
  "Inserts the result section of the current source block as the last message in an LLM chat.
Generates the chat string using `night/org-babel-result-to-chat-string` and inserts it at point."
  ;; (interactive) ;; to avoid polluting the interactive namespac
  (let ((chat-string (night/org-babel-result-to-chat-string)))
    (night/insert-for-yank (org-escape-code-in-string chat-string))))

(defun night/org-babel-insert-as-chat (&optional retry-attempted chat-string-arg)
  "Inserts the result section of the relevant source block as a chat message
within that source block itself.

If point is not within a source block initially, tries the previous
source block once.

Finds the last occurrence of '        \"\"\"},' in the target source
block's content and inserts the chat string generated by
`night/org-babel-result-to-chat-string` immediately after it.

RETRY-ATTEMPTED and CHAT-STRING-ARG are for internal recursive use."
  (interactive)
  ;; 1. Determine the chat string (only once, on the initial call)
  ;;    This uses the result potentially found by night/org-babel-result-get's retry.
  (let ((chat-string (or chat-string-arg
                         (night/org-babel-result-to-chat-string))))
    (unless chat-string
      ;; Use user-error as this is likely a user-fixable issue (no result executed/found)
      (user-error "Failed to generate chat string from results (current or previous block)"))

    ;; 2. Check current location and proceed/retry
    (let ((element (org-element-context)))
      (cond
       ;; --- Case 1: Found src-block at current location ---
       ((and element (eq (org-element-type element) 'src-block))
        (let* ((beg (org-element-property :begin element))
               (end (org-element-property :end element))
               ;; The string to search for: '        \"""},'
               ;; Needs escaping for Lisp: \ becomes \\, " becomes \"
               ;; Make sure the number of leading spaces is correct.
               (search-target " \"\"\"},")) ; Adjust leading spaces if necessary

          (unless (and beg end)
            ;; Use error for internal inconsistencies
            (error "Could not determine source block content boundaries"))

          ;; Use save-restriction for searching safely within the block
          (save-restriction
            (narrow-to-region beg end)
            (goto-char (point-max)) ; Start search from the very end of the block content
            (if (search-backward search-target (point-min) t) ; Search backward, bounded, no error on fail
                ;; Found: Insert the chat string
                (progn
                  (goto-char (match-end 0)) ; Move point to the end of the found string
                  ;; Insert newline + chat string.
                  ;; Assuming night/insert-for-yank handles indentation etc.
                  ;; and leaves point at the end of inserted text.
                  (night/insert-for-yank
                   (org-escape-code-in-string
                    (concat "\n" chat-string)))
                  (previous-line)       ; Move the point to the previous line so the user can start typing their next message immediately.
                  (evil-insert-state)
                  t) ; Indicate success for the cond branch

              ;; Not Found: Error
              (user-error "Could not find the target string '%s' in the source block." search-target)))))

       ;; --- Case 2: Not a src-block AND already retried ---
       (retry-attempted
        (user-error "Point is not in a source block, and no source block found previously."))

       ;; --- Case 3: Not a src-block AND first attempt ---
       (t
        (message "Point not in source block, trying previous...")
        ;; Move point to the beginning of the previous source block.
        ;; This changes the buffer's point.
        (night/org-babel-previous-src-block)
        ;; Recursively call, passing retry flag and the already generated chat string
        (night/org-babel-insert-as-chat t chat-string))))))
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
        "zK" #'night/org-babel-remove-all-results

        "za" #'night/org-babel-execute-maybe-and-backward
        "zx" #'night/org-babel-execute-maybe-and-forward
        "zs" #'org-babel-execute-subtree
        "zb" #'org-babel-execute-buffer
        "zn" #'night/org-babel-execute-after
        "zp" #'night/org-babel-execute-before
        "zz" #'org-ctrl-c-ctrl-c

        "zv" #'night/org-babel-select-src-and-results
        "zc"
        #'night/org-babel-insert-as-chat
        ;; #'night/org-babel-copy-as-chat
        "zC" #'night/org-babel-result-get

        "zl" #'night/org-blacken-region

        "zt" #'org-babel-tangle
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

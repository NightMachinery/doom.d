;;; autoload/org/babel/night-babel-statistics.el -*- lexical-binding: t; -*-

(defun night/org-babel-count-source-lines (arg)
  "Count the number of lines in all Org-babel source blocks and report them per language.
With a prefix argument ARG, print each source block separately. With two prefix arguments, print debugging messages."
  (interactive "P")
  ;; @LLM arg will be 4 if the command is run with one prefix argument (C-u) and 16 if the command is run with two prefix arguments (C-u C-u).
  (save-excursion
    (let ((block-count 0)
          (total-lines 0)
          (lang-lines-alist '())
          (buffer-name "*org-babel stats*")
          (debug
           (eq arg 16))) ;; If two prefix arguments, set debug to true
      (cl-labels
          ((night/add-to-alist
             (lang lines alist)
             "Increment the value associated with LANG in ALIST by LINES."
             (let* ((lang-entry (assoc lang alist))
                    (lang-total (if lang-entry (cdr lang-entry) 0)))
               (when debug
                 (message "Processing language %s with %d lines..." lang lines))
               (if lang-entry
                   (progn
                     (setcdr lang-entry (+ lang-total lines))
                     alist)
                 (when debug
                   (message "Processed language %s. New total: %d" lang (if lang-entry (cdr lang-entry) lines)))
                 (cons (cons lang lines) alist))))
           (night/display-summary
             (buffer-name alist total-lines)
             "Display the summary of line counts in the buffer specified by BUFFER-NAME."
             (with-output-to-temp-buffer buffer-name
               (princ "Line Count of Org-Babel Source Blocks (Empty Lines Omitted)\n-------------------------------------\n\n")
               (dolist (lang-entry alist)
                 (princ (format "%s: %d\n" (car lang-entry) (cdr lang-entry))))
               (princ (format "total: %d" total-lines))
               (when debug
                 (message "Summary displayed.")))))
        (goto-char (point-min))
        ;; Loop over all source blocks.
        (while (re-search-forward org-babel-src-block-regexp nil t)
          (cl-incf block-count)
          (let* ((info (org-babel-get-src-block-info 'light))
                 (lang (car info))
                 (body (cadr info))
                 (lines (night/count-lines-in-string body t)))
            (cl-incf total-lines lines)
            (setq lang-lines-alist (night/add-to-alist lang lines lang-lines-alist))
            (when arg
              (night/append-to-buffer buffer-name (format "Source block %d (lang: %s) has %d lines of code.\n" block-count lang lines))
              (when debug
                (message "Processed block %d with language %s and %d lines." block-count lang lines))))
          (night/display-summary buffer-name lang-lines-alist total-lines)
          (when debug
            (message "Finished counting.")))))))

(defun night/count-lines-in-string (str &optional skip-empty)
  "Count the number of lines in STR."
  (length (split-string str "\n" skip-empty)))

(defun night/append-to-buffer (buffer-name str)
  "Append STR to the buffer specified by BUFFER-NAME."
  (with-current-buffer (get-buffer-create buffer-name)
    (goto-char (point-max))
    (insert str)))

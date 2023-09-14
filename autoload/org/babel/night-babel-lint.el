;;; autoload/org/babel/night-babel-lint.el -*- lexical-binding: t; -*-

(defun night/pyblack-cat (code-to-format)
  "Format the region using the Black Python formatter."
  (let ((temp-file (make-temp-file "pyblack" nil ".py")))
    (with-temp-file temp-file
      (insert code-to-format))
    (with-temp-buffer
      (let* ((stdout-buffer (current-buffer))
             (exit-code
              (call-process "python" nil
                            stdout-buffer
                            nil
                            "-c"
                            "import sys\nfrom black import format_str, FileMode\nwith open(sys.argv[1], 'r') as f:\n  print(format_str(f.read(), mode=FileMode()), end=\"\")\n"
                            temp-file)))
        (cond
         ((zerop exit-code)
          (delete-file temp-file)
          (cons t (s-trim-right (buffer-string))))
         (t
          (delete-file temp-file)
          (cons nil (buffer-string)))
         )
        ))))

(comment
 (night/pyblack-cat "---")
 )

(defun night/org-babel-process (start end processor_fn language_pattern)
  "Process matching code blocks with `processor_fn`.
START and END specify the region of the org file to process. `processor_fn` receives a code block as a string and should return a tuple `(success_p . result)` where `success_p` indicates if the processing was successful and `result` contains the processed code. Blocks matching `language_pattern` will be processed."
  ;; @todo `end' needs to be adjusted as we change the content between start and end. Use `nil' for now.
  ;;;
  (save-excursion
    (let ((case-fold-search t)
          block-start block-end src-block code result success formatted-code)
      (goto-char start)
      (while (re-search-forward "^#\\+begin_src \\([^[:space:]]+\\)" end t)
        (when-let
            ((lang (match-string 1))
             (_matched (string-match-p language_pattern lang))
             (block-start (progn
                            (forward-line)
                            (point)))
             (block-end (- (progn
                             (if (re-search-forward "^#\\+end_src" end t)
                                 (line-beginning-position)
                               (error "No matching #+END_SRC found.")))
                           1))
             (src-block (org-element-at-point))
             (code (org-element-property :value src-block))
             (result (funcall processor_fn code)))
          (let ((success (car result))
                (formatted-code (cdr result)))
            (when success
              (delete-region block-start block-end)
              (goto-char block-start)
              (insert
               (org-escape-code-in-string formatted-code))
              (forward-line)
              )))
        ;; (setq end (min end (point-max)))
        ))))

(comment
 (defun night/org-babel-process-v1 (start end processor_fn language_pattern)
   "Process matching code blocks with `processor_fn`.
START and END specify the region of the org file to process. `processor_fn` receives a code block as a string and should return a tuple `(success_p . result)` where `success_p` indicates if the processing was successful and `result` contains the processed code. Blocks matching `language_pattern` will be processed."
   (org-element-map (org-element-parse-buffer) 'src-block
     (lambda (src-block)
       (when (string-match-p language_pattern (org-element-property :language src-block))
         (let* ((block-start (progn (goto-char (org-element-property :begin src-block))
                                    (+ 1 (line-end-position 1))))
                (block-end (progn (goto-char (org-element-property :end src-block))
                                  (line-beginning-position -1)))
                (code (org-element-property :value src-block))
                (result (funcall processor_fn code))
                (success (car result))
                (formatted-code (cdr result)))
           (when success
             (delete-region block-start block-end)
             (goto-char block-start)
             (insert
              ;; (org-escape-code-in-string formatted-code)
              "monkey"
              "\n"))))))))

(defun night/org-blacken-buffer ()
  "Format Python source blocks in the current org-mode buffer using Black."
  (interactive)
  (night/org-babel-process
   (point-min)
   ;; (point-max)
   nil
   #'night/pyblack-cat "\\(jupyter-\\)?python"))

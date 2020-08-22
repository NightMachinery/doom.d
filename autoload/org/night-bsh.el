;;; night-bsh.el ---                                 -*- lexical-binding: t; -*-

(require 'ob-shell)

(progn (add-to-list 'org-src-lang-modes '("zsh" . sh))
  (add-to-list 'org-src-lang-modes '("bsh.dash" . sh))
  (add-to-list 'org-babel-shell-names "bsh.dash")
  (org-babel-shell-initialize))

(comment
 (defadvice org-babel-sh-evaluate (around set-shell activate)
   "Add header argument :shcmd that determines the shell to be called."
   (let* ((org-babel-sh-command (or (cdr (assoc :shcmd params)) org-babel-sh-command shell-file-name))
          (shell-file-name org-babel-sh-command)
          )
     ad-do-it
     ))
 )

(comment


;;; Code:
 (require 'ob)
 (require 'ob-ref)
 (require 'ob-comint)
 (require 'ob-eval)
 ;; possibly require modes required for your language

 ;; optionally define a file extension for this language
 (add-to-list 'org-babel-tangle-lang-exts '("brishz" . "zsh"))

 ;; optionally declare default header arguments for this language
 (defvar org-babel-default-header-args:template '())

 ;; This function expands the body of a source code block by doing
 ;; things like prepending argument definitions to the body, it should
 ;; be called by the `org-babel-execute:template' function below.
 ;; (defun org-babel-expand-body:template (body params &optional processed-params)
 ;;   "Expand BODY according to PARAMS, return the expanded body."
 ;;   (require 'inf-template)
 ;;   (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
 ;;     (concat
 ;;      (mapconcat ;; define any variables
 ;;       (lambda (pair)
 ;;         (format "%s=%S"
 ;;                 (car pair) (org-babel-template-var-to-template (cdr pair))))
 ;;       vars "\n") "\n" body "\n")))

 ;; This is the main function which is called to evaluate a code
 ;; block.
 ;;
 ;; This function will evaluate the body of the source code and
 ;; return the results as emacs-lisp depending on the value of the
 ;; :results header argument
 ;; - output means that the output to STDOUT will be captured and
 ;;   returned
 ;; - value means that the value of the last statement in the
 ;;   source code block will be returned
 ;;
 ;; The most common first step in this function is the expansion of the
 ;; PARAMS argument using `org-babel-process-params'.
 ;;
 ;; Please feel free to not implement options which aren't appropriate
 ;; for your language (e.g. not all languages support interactive
 ;; "session" evaluation).  Also you are free to define any new header
 ;; arguments which you feel may be useful -- all header arguments
 ;; specified by the user will be available in the PARAMS variable.
 (defun org-babel-execute:template (body params)
   "Execute a block of Template code with org-babel.
This function is called by `org-babel-execute-src-block'"
   (message "executing Template source code block")
   (let* ((processed-params (org-babel-process-params params))
          ;; set the session if the session variable is non-nil
          (session (org-babel-template-initiate-session (first processed-params)))
          ;; variables assigned for use in the block
          (vars (second processed-params))
          (result-params (third processed-params))
          ;; either OUTPUT or VALUE which should behave as described above
          (result-type (fourth processed-params))
          ;; expand the body with `org-babel-expand-body:template'
          (full-body (org-babel-expand-body:template
                      body params processed-params)))
     ;; actually execute the source-code block either in a session or
     ;; possibly by dropping it to a temporary file and evaluating the
     ;; file.
     ;;
     ;; for session based evaluation the functions defined in
     ;; `org-babel-comint' will probably be helpful.
     ;;
     ;; for external evaluation the functions defined in
     ;; `org-babel-eval' will probably be helpful.
     ;;
     ;; when forming a shell command, or a fragment of code in some
     ;; other language, please preprocess any file names involved with
     ;; the function `org-babel-process-file-name'. (See the way that
     ;; function is used in the language files)
     ))

 ;; This function should be used to assign any variables in params in
 ;; the context of the session environment.
 (defun org-babel-prep-session:template (session params)
   "Prepare SESSION according to the header arguments specified in PARAMS."
   )

 (defun org-babel-template-var-to-template (var)
   "Convert an elisp var into a string of template source code
specifying a var of the same value."
   (format "%S" var))

 (defun org-babel-template-table-or-string (results)
   "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
   )

 (defun org-babel-template-initiate-session (&optional session)
   "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
   (unless (string= session "none")
     ))

 (provide 'ob-brishz)
;;; ob-template.el ends here
 )

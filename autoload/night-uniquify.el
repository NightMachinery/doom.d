;;; night-uniquify.el --- Additional uniquify styles -*- lexical-binding: t -*-

(after! (uniquify)
  ;; Configuration variables
  (defgroup night/uniquify nil
    "Customization options for night-uniquify."
    :group 'uniquify)

  (defcustom night/uniquify-verbosity-level 0
    "Control verbosity of logging messages.
0 means silent, 1 means basic logging, 2 means detailed logging."
    :type 'integer
    :group 'night/uniquify)

  (defcustom night/uniquify-always-show-depth 2
    "Number of parent directories to show in buffer name for uniqueness.
Default is 2."
    :type 'integer
    :group 'night/uniquify)

  (defvar night/h-uniquify-buffer-name-style nil
    "Internal variable to hold night-specific uniquify buffer naming style.")

  (defun night/set-uniquify-buffer-name-style (style)
    "Set the uniquify buffer naming style, with support for 'always-forward.
This function wraps setting an internal variable `night/h-uniquify-buffer-name-style`
and ensures `uniquify-buffer-name-style` only uses standard values."
    (setq night/h-uniquify-buffer-name-style style)
    (setq uniquify-buffer-name-style
          (if (eq style 'always-forward) 'forward style)))

  ;; Internal functions
  (defun night/h-log (level message &rest args)
    "Log MESSAGE with ARGS if night/uniquify-verbosity-level >= LEVEL."
    (when (>= night/uniquify-verbosity-level level)
      (apply #'message (concat "night-uniquify: " message) args)))

  (defun night/h-get-parent-dirs (dirname depth)
    "Get up to DEPTH parent directory names from DIRNAME, handling edge cases."
    (let ((parts ()))
      (condition-case err
          (while (and dirname (> depth 0))
            (let ((parent (file-name-nondirectory (directory-file-name dirname))))
              (if (string-empty-p parent)
                  (setq dirname nil)
                (setq parts (cons parent parts)
                      dirname (file-name-directory (directory-file-name dirname))
                      depth (1- depth)))))
        (error
         (night/h-log 1 "Error getting parent dirs: %S" err)))
      (string-join parts "/")))

  ;; Main functionality
  (defun night/h-get-proposed-name (base dirname)
    "Generate proposed buffer name showing parent directories up to `night/uniquify-always-show-depth`.
BASE is the original buffer name.
DIRNAME is the full directory path."
    (condition-case err
        (progn
          (night/h-log 2 "Processing name for base=%S dirname=%S" base dirname)

          (if (or (null dirname) (string-empty-p base))
              base                      ; Handle edge cases
            (let ((parents (night/h-get-parent-dirs dirname night/uniquify-always-show-depth)))
              (if (string-empty-p parents)
                  base
                (concat parents "/" base)))))
      (error
       (night/h-log 1 "Error in get-proposed-name: %S" err)
       base)))                          ; Fallback to original name on error

  ;; Add our style via advice
  (defun night/uniquify-get-proposed-name-advice (orig-fun base dirname &optional depth original-dirname)
    "Advice to add always-forward style to uniquify.
Calls original function for other styles."
    (let ((orig
           (funcall orig-fun base dirname depth original-dirname)))
      (cond
       ((and
         (eq night/h-uniquify-buffer-name-style 'always-forward)
         ;; path separator char not in `orig':
         (not (string-match-p "/" orig))
         )
        ;; (message "Night: uniquify-get-proposed-name: base=%S dirname=%S depth=%S original-dirname=%S" base dirname depth original-dirname)
        (night/h-get-proposed-name base dirname))
       (t
        orig))))

  ;; Install our advice
  (advice-add 'uniquify-get-proposed-name :around #'night/uniquify-get-proposed-name-advice)
  ;;;
  ;; Set the desired style:
  (night/set-uniquify-buffer-name-style 'always-forward)
  ;;;
  (provide 'night-uniquify)
;;; night-uniquify.el ends here
  )

;;; autoload/night-dired.el -*- lexical-binding: t; -*-

(require 'dired+)

(after! (dired-aux dired+)
  ;; @userConfig
  ;;   - `dired-create-destination-dirs'
;;;
  (unbind-key "M-DEL" dired-mode-map)
  (unbind-key "M-O" dired-mode-map)
  (unbind-key "M-<left>" dired-mode-map)
  (unbind-key "M-<right>" dired-mode-map)

  (map! :map dired-mode-map
        :nv "Y" #'diredp-copy-abs-filenames-as-kill
        :nv "{" #'diredp-copy-abs-filenames-as-kill-recursive
        )

  (remove-hook 'dired-mode-hook #'dired-omit-mode)
;;; @toFuture/1401 @patchSent
  (defun dired-do-create-files (op-symbol file-creator operation arg
					  &optional marker-char op1
					  how-to)
    "Create a new file for each marked file.
Prompt user for a target directory in which to create the new
  files.  The target may also be a non-directory file, if only
  one file is marked.  The initial suggestion for target is the
  Dired buffer's current directory (or, if `dired-dwim-target' is
  non-nil, the current directory of a neighboring Dired window).
OP-SYMBOL is the symbol for the operation.  Function `dired-mark-pop-up'
  will determine whether pop-ups are appropriate for this OP-SYMBOL.
FILE-CREATOR and OPERATION as in `dired-create-files'.
ARG as in `dired-get-marked-files'.
Optional arg MARKER-CHAR as in `dired-create-files'.
Optional arg OP1 is an alternate form for OPERATION if there is
  only one file.
Optional arg HOW-TO determines how to treat the target.
  If HOW-TO is nil, use `file-directory-p' to determine if the
   target is a directory.  If so, the marked file(s) are created
   inside that directory.  Otherwise, the target is a plain file;
   an error is raised unless there is exactly one marked file.
  If HOW-TO is t, target is always treated as a plain file.
  Otherwise, HOW-TO should be a function of one argument, TARGET.
   If its return value is nil, TARGET is regarded as a plain file.
   If it return value is a list, TARGET is a generalized
    directory (e.g. some sort of archive).  The first element of
    this list must be a function with at least four arguments:
      operation - as OPERATION above.
      rfn-list  - list of the relative names for the marked files.
      fn-list   - list of the absolute names for the marked files.
      target    - the name of the target itself.
    The rest of elements of the list returned by HOW-TO are optional
    arguments for the function that is the first element of the list.
   For any other return value, TARGET is treated as a directory."
    (or op1 (setq op1 operation))
    (let* ((fn-list (dired-get-marked-files nil arg nil nil t))
	   (rfn-list (mapcar #'dired-make-relative fn-list))
	   (dired-one-file            ; fluid variable inside dired-create-files
	    (and (consp fn-list) (null (cdr fn-list)) (car fn-list)))
	   (target-dir (dired-dwim-target-directory))
	   (default (and dired-one-file
		         (not dired-dwim-target) ; Bug#25609
		         (expand-file-name (file-name-nondirectory (car fn-list))
					   target-dir)))
	   (defaults (dired-dwim-target-defaults fn-list target-dir))
	   (target (expand-file-name  ; fluid variable inside dired-create-files
		    (minibuffer-with-setup-hook
		        (lambda ()
                          (setq-local minibuffer-default-add-function nil)
			  (setq minibuffer-default defaults))
		      (dired-mark-read-file-name
                       (format "%s %%s %s: "
                               (if dired-one-file op1 operation)
                               (if (memq op-symbol '(symlink hardlink))
                                   ;; Linking operations create links
                                   ;; from the prompted file name; the
                                   ;; other operations copy (etc) to the
                                   ;; prompted file name.
                                   "from" "to"))
		       target-dir op-symbol arg rfn-list default))))
	   (into-dir
            (progn
              (when (or (not dired-one-file)
                        (string= (substring target -1) "/"))
                (dired-maybe-create-dirs target))
              (cond ((null how-to)
                     ;; Allow users to change the letter case of
                     ;; a directory on a case-insensitive
                     ;; filesystem.  If we don't test these
                     ;; conditions up front, file-directory-p
                     ;; below will return t on a case-insensitive
                     ;; filesystem, and Emacs will try to move
                     ;; foo -> foo/foo, which fails.
		     (if (and (file-name-case-insensitive-p (car fn-list))
			      (eq op-symbol 'move)
			      dired-one-file
			      (string= (downcase
				        (expand-file-name (car fn-list)))
				       (downcase
				        (expand-file-name target)))
			      (not (string=
				    (file-name-nondirectory (car fn-list))
				    (file-name-nondirectory target))))
		         nil
		       (file-directory-p target)))
		    ((eq how-to t) nil)
		    (t (funcall how-to target))))))
      (if (and (consp into-dir) (functionp (car into-dir)))
	  (apply (car into-dir) operation rfn-list fn-list target (cdr into-dir))
        (if (not (or dired-one-file into-dir))
	    (error "Marked %s: target must be a directory: %s" operation target))
        (if (and (not (file-directory-p (car fn-list)))
                 (not (file-directory-p target))
                 (directory-name-p target))
            (error "%s: Target directory does not exist: %s" operation target))
        ;; rename-file bombs when moving directories unless we do this:
        (or into-dir (setq target (directory-file-name target)))
        (prog1
            (dired-create-files
             file-creator operation fn-list
             (if into-dir               ; target is a directory
                 ;; This function uses fluid variable target when called
                 ;; inside dired-create-files:
	         (lambda (from)
	           (expand-file-name (file-name-nondirectory from) target))
	       (lambda (_from) target))
             marker-char)
          (when (or (eq dired-do-revert-buffer t)
                    (and (functionp dired-do-revert-buffer)
                         (funcall dired-do-revert-buffer target)))
            (dired-fun-in-all-buffers (file-name-directory target) nil
                                      #'revert-buffer))))))
;;;
  )

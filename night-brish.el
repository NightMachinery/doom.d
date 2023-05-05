;;; autoload/night-brish.el -*- lexical-binding: nil; -*-
;;; @todo2 z needs `lexical-binding: nil` on its users as well

(defun night/h-brishz-arg (arg &optional no-eval)
  (cond
   ((symbolp arg)
    (symbol-name arg))
   ((listp arg)
    (mycomment only called from -z-helper as night/brishz flattens its arguments)
    (cond
     (no-eval arg)
     (t (user-error! "List encountered outside of the macro: %s" arg)))
    )
   (t
    (format "%s" arg)
    )))

(defun night/h-brishz-arg-macro (arg)
  (night/h-brishz-arg arg t)
  )

(defvar *brishz-retcode* 0 "A global reply variable that holds the return code of the last invocation of night/brishz")

(defun night/brishz? ()
  "Whether the last night/brishz invocation returned zero."
  (interactive)
  (= *brishz-retcode* 0))

(defun night/brishz (&rest args)
  ;; @todo1 @perf add night/brishz-eval that uses elisp to make the HTTP requests
  ;; then use it in night/ivy-docstring
;;;
  (interactive
   (let ((cmd (read-string "Command: " nil 'night-brishz-history)))
     (list "eval" cmd)))
  (apply #'night/brishz-in-session "" args))

;;;
(cl-defun night/call-process-async
    (&key
       command
       (callback nil) ;; nil means synchronous, use t for async with no callback
       (stdout-trim-right t)
       (name "night/call-process-async")
       (stdin-text nil)
       (error-callback nil))

  ;; (setq-local lexical-binding t) ;; @useless
  (lexical-let* ((stdout-trim-right stdout-trim-right)
                 (output-buffer
                  (generate-new-buffer
                   (concat " *" name "*")
                   t))
                 (stderr-buffer
                  (generate-new-buffer
                   (concat " *" name "_stderr*")
                   t))
                 (callback-fun callback)
                 (sentinel
                  (lambda (process signal)
                    (when (memq (process-status process) '(exit signal))
                      (when
                          (not (or
                                (equal callback-fun t)
                                (null callback-fun)))
                        (let*
                            ((stdout
                              (with-current-buffer output-buffer
                                (buffer-substring-no-properties (point-min) (point-max))))
                             (stdout
                              (cond
                                (stdout-trim-right
                                 (s-trim-right stdout))
                                (t stdout)))
                             (stderr
                              (with-current-buffer stderr-buffer
                                (buffer-substring-no-properties (point-min) (point-max)))))
                          (funcall callback-fun
                                   stdout
                                   (process-exit-status process)
                                   stderr)))
                      (kill-buffer output-buffer)
                      (kill-buffer stderr-buffer)))))
    (let ((process
           (make-process
            :name name
            :buffer output-buffer
            :stderr stderr-buffer
            :command command
            :connection-type 'pipe
            :sentinel sentinel))
          (process-error nil))
      (when stdin-text
        (condition-case err
            (progn
              (process-send-string process stdin-text)
              (process-send-eof process))
          (error (setq process-error err))))
      (when (and process-error error-callback)
        (funcall error-callback process-error))
      output-buffer)))

(comment
 (night/call-process-async
  :command '("tac")
  :callback (lambda (stdout exitcode stderr)
              (message "Output: %s" stdout))
  :stdin-text "Apple\nZorg\nBoring\n"))
;;;
;; sets globally
(setenv "brishz_out_file_p" "y")
(setenv "brishz_eval_file_p" "y")

(defun night/brishz-in-session (session &rest args)
  (night/brishz-ll
   :session session
   :command args))

(cl-defun night/brishz-ll
    (&key
     session
     command
     callback
     (name "night/brishz-ll")
     (stdout-trim-right t))
  (interactive
   (let ((session (read-string "Session: " nil 'night-brishz-session-history))
         (cmd (read-string "Command: " nil 'night-brishz-history)))
     (list session "eval" cmd)))
  (let* ((args command)
         (str-args '())
         (process-environment process-environment))

    (when (and (not (equalp session "")))
      (setq process-environment (cl-copy-list process-environment))
      (setenv "brishz_session" session))

    (dolist (arg (-flatten args))
      (push (night/h-brishz-arg arg) str-args))
    (setq str-args (nreverse str-args))

    (cond
     ((not (or
            (null callback)))
      (night/call-process-async
       :name name
       :command (cons "brishzq.zsh" str-args)
       :callback callback
       :stdout-trim-right stdout-trim-right))
     (t
      (let*
          ((errbuffname "*brishz-err*")
           (errbuff (get-buffer-create errbuffname))
           (error-file (make-temp-file "brishz-err-file"))
           (stdout (with-output-to-string
                     (setq *brishz-retcode* (apply #'call-process "brishzq.zsh" nil (list standard-output error-file) nil str-args)
                           ;; @singleThreaded Emacs Lisp has no real multithreading, so it is safe to store results inside private global variable.
                           )


                     ;; display errors, stolen from emacs' `shell-command` function, stolen from https://emacs.stackexchange.com/questions/12450/capturing-stderr-of-subprocesses?noredirect=1&lq=1
                     (when (and error-file (file-exists-p error-file))
                       (if (< 0 (nth 7 (file-attributes error-file)))
                           (with-current-buffer errbuff
                             (let ((pos-from-end (- (point-max) (point))))
                               (or (bobp)

                                   ;; \f skips to the start of the next page.
                                   ;; (insert "\f\n")
                                   (insert "\n\n=================\n=================\n\n"))
                               ;; Do no formatting while reading error file,
                               ;; because that can run a shell command, and we
                               ;; don't want that to cause an infinite recursion.
                               (format-insert-file error-file nil)
                               ;; Put point after the inserted errors.
                               (goto-char (- (point-max) pos-from-end)))
                             (+popup/buffer))))))
           (stdout
            (cond
             (stdout-trim-right
              (s-trim-right stdout))
             (t stdout))))
        stdout)))))

(defmacro z (&rest args)
  (-concat (list '-z-helper ) (mapcar #'night/h-brishz-arg-macro args)))

(defun -z-helper (&rest args)
  (s-trim-right (night/brishz (mapcar #'night/h-brishz-arg (-flatten args)))))

(defmacro z-async (callback &rest args)
  (-concat (list '-z-helper-async callback) (mapcar #'night/h-brishz-arg-macro args)))

(defun -z-helper-async (callback &rest args)
  (night/brishz-ll
   :command (mapcar #'night/h-brishz-arg (-flatten args))
   :callback callback))

(defmacro zf (&rest args)
  (list 'split-string (append '(z) args) "\n" t)
  )

(defmacro z0 (&rest args)
  (list 'split-string (append '(z) args) "\0" t))

(defmacro zb (&rest args)
  ;; (list 'progn (append '(z) args) '(night/brishz?))
  ;; this method triggered a failure on BrishGarden
  ;;;
  (list 'equalp "0" (append '(z) (list "reval-retcode") args)))

(defalias 'i 'identity)
;;; tests:
(defun -z-test1 ()
  (interactive)
  (message "%s" (z ec (buffer-file-name))))

(mycomment
 (night/brishz-ll
  :command (list "eval" "fsay interesting ; date ; ecerr some_error")
  :callback (lambda (out ret err &rest dummy)
              (message "async process ended! ret: %s\nout: %s\nerr: %s" ret out err)))
 (z-async
  (lambda (out ret err &rest dummy)
    (message "async process ended! ret: %s\nout: %s\nerr: %s" ret out err))
  eval "bello ; ec Hi && sleep 1 && ecerr my bad")
 (z-async
  (lambda (out ret err &rest dummy)
    (message "async process ended! ret: %s\nout: %s\nerr: %s" ret out err))
  ec (* 7 9))

 (z-async t bell-fail)
 (z-async t fsay what a big long day)
 (z-async t bell-ddd)
 (z bell-ddd)

 (zb true)
 (zb false)

 (z ecn hi)
 (z "ecn" hi)
 (night/brishz-in-session "hi" "ec" "wow")

 (z ecerr moo)
 (z ec (i gc-cons-percentage))
 (setq a (z ecerr moo))

 (z eval "arrN {1..20} >&2 ; echo -n Done';)'")

 (z du -h (split-string (z ls -a) "\n" t))

 (z du -h (zf ls -a))

 (z rin (z arrN (z0 arr0 1 2 3)) prefixer -o ", "))
;;;
(defun night/brishz-eval-region (beg end)
  (interactive "r")
  (save-mark-and-excursion
    (let*
        ((code
          (buffer-substring-no-properties beg end))
         (result (s-trim-right (night/brishz-in-session "emacs" "eval" code))))
      (if (< (length result) 50)
          (eros--eval-overlay
           ;; (format "res: %s" code)
           result
           end)
        (progn
          (comment (lispy-message result t))
          (progn (let ((bn "*brishz-output*"))
                     (ignore-errors (kill-buffer bn))
                     (let ((b (get-buffer-create bn)))
                       (with-current-buffer b
                         (insert result)
                         (goto-char (point-min))

                         ;; (special-mode)
                         (+word-wrap-mode)
                         (read-only-mode)

                         (pop-to-buffer b)
                         (doom/toggle-line-numbers)
                         )))))))))

(defun night/brishz-eval-line ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'night/brishz-eval-region)
    (save-mark-and-excursion
      (my-select-current-line)
      (call-interactively #'night/brishz-eval-region))))

(defun night/brishz-eval-cell ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'night/brishz-eval-region)
    (save-mark-and-excursion
      (-if-let* ((cell (night/cell-get))
                 (beg (car cell))
                 (end (cadr cell)))
          (night/brishz-eval-region beg end)))))

(defun night/region-get-regexp (opening closing)
  (let ( (end (save-excursion ; using save-excursion because
                                        ; we don't want to move the
                                        ; point.
                (re-search-forward closing nil t))) ; bound nil
                                        ; no-error t
         (beg (save-excursion (re-search-backward opening nil t))))
    (when (and end beg)
      (list beg end)
      )))

(defun night/cell-get ()
  (let ((cell-marker "^\s*##+"))
    (night/region-get-regexp cell-marker cell-marker)))

(defun night/cell-select ()
  (interactive)
  (-if-let* ((cell (night/cell-get))
             (beg (car cell))
             (end (cadr cell)))
      (progn (push-mark end)
             (goto-char beg)
             (activate-mark))))

(after!
  (sh-script)
  ;; (z bello)
  (map! :map sh-mode-map
        :localleader
        "er" #'night/brishz-eval-region
        "ee" #'night/brishz-eval-line
        "ec" #'night/brishz-eval-cell))
;;;
(defun night/brishz-doc-at-point ()
  "Show help for the symbol at point."
  (interactive)
  ;; (z fsay okay)
  (ignore-errors (kill-buffer buf-name))
  (-if-let (symbol (symbol-at-point))
      (let* ((doc (night/brishz-in-session "emacs" "whichm" symbol))
             (buf-name "*help-brishz-doc*")
             ;; the name should start with '*help' to trigger doom's heuristics:
             ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/popup/README.org#set-popup-rule-and-set-popup-rules

             (buf (get-buffer-create buf-name))
             )

        ;; (z fsay forest)
        (with-current-buffer buf
          (sh-mode)
          ;;;
          ;; I don't know how to make sh-mode realize it's in zsh mode, so I have disabled flycheck as a temporary workaround not to see the ugly useless red errors
          (flycheck-mode -1)
          ;;;
          (erase-buffer)
          (insert doc)
          (goto-char (point-min))
          ;; (+popup/buffer)
          )

        (pop-to-buffer buf)
        t
        )
    (user-error "There is no symbol at point.")))

(after! sh-script
  (set-lookup-handlers! 'sh-mode :documentation #'night/brishz-doc-at-point)
  ;; the args are incompatible
  ;; (advice-add '+sh-lookup-documentation-handler :override #'brishz/doc-at-point)
  )
;;;
(cl-defun night/brishz-async-insert
    (&key
     command
     (name "night/brishz-async-insert")
     (callback-after #'night/bello)
     (callback-failure #'night/bell-fail)
     (insert-fn night/org-insert-and-fix-levels)
     (save-p t)
     (paste-on-error-p t))
  (lexical-let*
      ((default-directory "/") ;; stop remote possible execution
       (callback-after callback-after)
       (insert-fn insert-fn)
       (save-p save-p)
       (paste-on-error-p paste-on-error-p)
       (name name)
       (overlay nil)
       (clipboard-content (night/pbpaste))
       (current-marker (point-marker))
       (current-buffer (current-buffer))
       (callback
        (lambda (out ret err &rest dummy)
          (let
              ((error-p (not
                         (equalp ret 0))))
            (when (or
                   error-p
                   (equalp out "")
                   (not
                    (equalp err "")))
              (message
               "%s: ret: %s\nout: %s\nerr: %s"
               name ret out err))
            (save-excursion
              (with-current-buffer current-buffer
                (goto-char current-marker)
                (delete-overlay overlay)
                (+nav-flash/blink-cursor)
                (when (and
                       paste-on-error-p
                       error-p
                       (not (equalp clipboard-content "")))
                  (insert-for-yank
                   (concat
                    ;; "An error occurred. The clipboard contained this when this function was called:\n"
                    "@asyncError/" name " clipboard:\n#+begin_example\n"
                    clipboard-content
                    "\n#+end_example\n")))
                (funcall insert-fn out)
                (when save-p
                  (night/save-buffer :force-normal-state nil))
                (funcall callback-after)))))))
    (progn
      (night/jump-set)
      (+nav-flash/blink-cursor)
      ;; The overlay can't be drawn when the caret is at the end of the file, so we should also flash the current line.
      (let
          ((pos (point))
           (start nil)
           (end nil))
        (save-excursion
          (let ((inhibit-point-motion-hooks t)
                (cursor-intangible-mode nil)
                (cursor-sensor-mode nil))
            (goto-char pos)
            (beginning-of-visual-line)
            (setq start (point))
            (end-of-visual-line)
            (setq end (1+ (point)))))
        (setq overlay
              (night/ov
               ;; :beg start
               :beg pos
               :end end
               :properties (list
                            'face
                            'night/async-insertion-face
                            'ov-night-async 4999)
               :front-advance nil
               :rear-advance nil
               ;; :front-advance t
               ;; :rear-advance t
               ))))
    (night/brishz-ll
     :name name
     :command command
     :callback callback)))
;;;

;;; autoload/night-brish.el -*- lexical-binding: t; -*-

(defun night/h-brishz-arg (arg)
  (cond
   ((symbolp arg)
    (symbol-name arg))
   ((listp arg)
    (comment only called from -z-helper as night/brishz flattens its arguments)
    (eval arg)
    )
   (t
    (format "%s" arg)
    )))

(defun night/brishz (&rest args)
  ;; @todo1 @perf add night/brishz-eval that uses elisp to make the HTTP requests
  ;; then use it in night/ivy-docstring
;;;
  (interactive
   (let ((cmd (read-string "Command: " nil 'night-brishz-history)))
     (list "eval" cmd)))
  (apply #'night/brishz-in-session "" args)
  )
(defun night/brishz-in-session (night/brishz-session &rest args)
  (interactive
   (let ((session (read-string "Session: " nil 'night-brishz-session-history))
         (cmd (read-string "Command: " nil 'night-brishz-history)))
     (list session "eval" cmd)
     )
   )
  (let* ((str-args '())
         (errbuffname "*brishz-err*")
         (errbuff (get-buffer-create errbuffname))
         (error-file (make-temp-file "brishz-err-file"))
         (process-environment process-environment))

    (when (and (not (equalp night/brishz-session "")))
      (setq process-environment (cl-copy-list process-environment))
      (setenv "brishz_session" night/brishz-session))

    (dolist (arg (-flatten args))
      (push (night/h-brishz-arg arg) str-args))

    (with-output-to-string
      (apply #'call-process "brishzq.zsh" nil (list standard-output error-file) nil (nreverse str-args))


      ;; display errors, stolen from emacs' `shell-command` function, stolen from https://emacs.stackexchange.com/questions/12450/capturing-stderr-of-subprocesses?noredirect=1&lq=1
      (when (and error-file (file-exists-p error-file))
        (if (< 0 (nth 7 (file-attributes error-file)))
            (with-current-buffer errbuff
              (let ((pos-from-end (- (point-max) (point))))
                (or (bobp)

                    ;; \f skips to the start of the next page.
                    ;; (insert "\f\n")
                    (insert "\n\n=================\n=================\n\n")

                    )
                ;; Do no formatting while reading error file,
                ;; because that can run a shell command, and we
                ;; don't want that to cause an infinite recursion.
                (format-insert-file error-file nil)
                ;; Put point after the inserted errors.
                (goto-char (- (point-max) pos-from-end)))
              (+popup/buffer)
              ))))))

;; (defmacro z (&rest args)
;;   (-concat (list 'night/brishz ) (mapcar #'night/h-brishz-arg (-flatten (mapcar #'night/h-brishz-arg args)))))
(defmacro z (&rest args)
  (-concat (list '-z-helper (list 'quote args))))

(defun -z-helper (args)
  (night/brishz (mapcar #'night/h-brishz-arg (-flatten (mapcar #'night/h-brishz-arg args))))
  )

(defmacro zf (&rest args)
  (list 'split-string (append '(z) args) "\n" t)
  )
(defmacro z0 (&rest args)
  (list 'split-string (append '(z) args) "\0" t)
  )
(defmacro mycomment (&rest a)
t
)

(defalias 'i 'identity)
;;; tests:
(defun -z-test1 ()
  (interactive)
  (z ec (buffer-file-name)))
(mycomment
 (z ecn hi)
 (z "ecn" hi)
 (night/brishz-in-session "hi" "ec" "wow")

 (z ecerr moo)
 (z ec (i gc-cons-percentage))
 (setq a (z ecerr moo))

 (z eval "arrN {1..20} >&2 ; echo -n Done';)'")

 (z du -h (split-string (z ls -a) "\n" t))

 (z du -h (zf ls -a))

 (z rin (z arrN (z0 arr0 1 2 3)) prefixer -o ", ")

 )
;;;
(defun night/brishz-eval-region (beg end)
  "Wrapper for `eval-last-sexp' that overlays results."
  (interactive "r")
  (save-mark-and-excursion
    (let*
        ((code
          (buffer-substring-no-properties beg end)
          ))
      (eros--eval-overlay
       ;; (format "res: %s" code)
       (s-trim-right (night/brishz-in-session "emacs" "eval" code))
       end))))

(defun night/brishz-eval-line ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'night/brishz-eval-region)
    (save-mark-and-excursion
      (my-select-current-line)
      (call-interactively #'night/brishz-eval-region))))

(map! :map sh-mode-map
      :localleader
      "er" #'night/brishz-eval-region
      "ee" #'night/brishz-eval-line
      )
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

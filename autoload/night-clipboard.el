;;; ~/doom.d/night-clipboard.el -*- lexical-binding: t; -*-
;;;
(require 'clipetty)
;; (global-clipetty-mode 1)
(comment
 ;;[[id:19422cda-0bda-412f-96b4-5026f5563955][spudlyo/clipetty: Manipulate the system (clip)board with (e)macs from a (tty)]]
 (defun night/osc52-paste ()
   (clipetty--emit
    (concat clipetty--osc-start "?" clipetty--osc-end))))
;;;
(defun night/h-kill-skip-whitespace (orig-fn string &optional rest)
  "an advice around `kill-new' to skip whitespace-only kills. @warn This can break some assumptions."
  (let* (
         (string-raw (substring-no-properties string))
         (string
          (progn
            (cond
             ((and
               (equalp major-mode 'org-mode)
               (> (length (split-string string-raw "\n" nil)) 1))
              (org-unescape-code-in-string string))
             (t string))))
         (skip-p
          (not (string-match-p "[^][ *-+_(){}\t\n\r]" ;; @note double backslashes are not needed or accepted here.
                               string-raw))
          ;; (or (equalp string-raw "")
          ;;     (string-match-p "^\\(\s\\|\n\\)+$" string-raw) ;; '^', '$' are treated per line, so this won't work
          ;;     )
          ))
    ;; (message "skip-p: %s, isSpace: %s, string-raw: %s, string-cat: %s" skip-p (zb isSpace (i string-raw)) string-raw (z reval-withstdin (i string-raw) cat -vte))
    (cond
     ((or
       ;; t ;; disable this modification entirely
       (not skip-p))
      (progn
        (apply orig-fn string rest)
        (when (night/ssh-p)
          ;; could have used [help:interprogram-cut-function]
          (night/call-process-async
           :command '("socat" "-" "TCP:127.0.0.1:6030")
           :callback (lambda (stdout exitcode stderr)
                       )
           :stdin-text string-raw))))
     (t
      ;; (message "skipped whitespace kill: %s" string-raw)
      (message "skipped whitespace kill")
      ;; This message allows us to be sure that we have been deleting whitespace lines and not hidden (folded) text.

      ;; imitating the return value of `kill-new'
      0))))
(advice-add 'kill-new :around #'night/h-kill-skip-whitespace)
;; @seeAlso [help:filter-buffer-substring]
;;;
(defun night/h-current-kill (orig-fun &rest args)
  (let ((result (apply orig-fun args)))
    (if (and (stringp result))
        (cond
         ((and
           nil
           ;; @disabled This introduced too many edge cases, unlike its copying counterpart in `kill-new'.
           (equalp major-mode 'org-mode)
           (string-match-p "\n" result))
          (org-escape-code-in-string result))
         (t result))
      result)))
(advice-add 'current-kill :around #'night/h-current-kill)
;;;
(defun ns-yank-image-at-point-as-image ()
  "Yank the image at point to the X11 clipboard as image/png."
  (interactive)
  (let ((image (get-text-property (point) 'display)))
    (if (eq (car image) 'image)
        (let ((data (plist-get (cdr image) ':data))
              (file (plist-get (cdr image) ':file)))
          (cond (data
                 (with-temp-buffer
                   (insert data)
                   (call-shell-region
                    (point-min) (point-max)
                    "impbcopy"))) ;; http://www.alecjacobson.com/weblog/?p=3816 Linux x11: https://emacs.stackexchange.com/questions/41016/how-can-i-yank-images-from-emacs?noredirect=1#comment64407_41016
                (file
                 (if (file-exists-p file)
                     (start-process
                      "_" nil "impbcopy"  (file-truename file))))
                (t
                 (message "The image seems to be malformed."))))
      (message "Point is not at an image."))))


;;;
(defun night/org-insert-and-fix-levels (text &optional level)
  "@seeAlso [agfi:org-header-indent-to-current]"
  (let*
      ((level
        (or level
            (org-current-level) 0)))
    (night/insert-for-yank
     (cond
      ((night/brish-p)
       (z reval-withstdin
          (identity text)
          h-org-insert-and-fix-levels (identity level)
          ;; perl -lpe (concat "s/^(?=\\*)/" (s-repeat level "*") "/g")
          ))
      (t text
         ;; @gracefulFallback
         )))))

(defun night/pbpaste ()
  "Returns the last copied string."
  (current-kill 0))

(defun night/pbcopy (&optional text)
  (interactive)
  (if text
      (kill-new text)
    (call-interactively #'evil-yank)))

(defun night/pbcopy-org2html ()
  (interactive)
  (night/pbcopy)
  (z reval-paste pbcopy-org2html))

(defun night/org-paste-yank ()
  (interactive)
  (night/org-insert-and-fix-levels
   (night/pbpaste)))

(defun night/paste-yank-html (&optional arg)
  (interactive "P")

  (night/org-insert-and-fix-levels
   (cond
    (arg
     (z eval "pbpaste-html-urlfinal | html2org"))
    (t
     (z html2org)))))

(defun night/insert-for-yank (text)
  (interactive)
  (night/evil-region-delete-on-visual-paste)
  (insert-for-yank text))

(defun night/insert-for-yank-and-save (text)
  (save-excursion
    (night/insert-for-yank text))
  (recenter 0)
  (save-buffer))
;;;
(after! (org evil-org) (map!
                        :map evil-org-mode-map
                        :nv "P" #'night/paste-yank-html
                        :nv "p" #'night/org-paste-yank))

(map! :leader
      "," nil)
(night/set-leader-keys ", h" #'night/paste-yank-html)
(night/set-leader-keys ", n" #'night/pns)
(night/set-leader-keys ", m" #'night/paste-md2org)
(night/set-leader-keys ", c" #'night/p-titlecase)
(night/set-leader-keys ", i" #'ns-yank-image-at-point-as-image)
;;;
(defun night/pbadd-current ()
  (interactive)
  (z pbadd (buffer-file-name)))
;;;
(defun night/region-copy ()
  "Copy the currently selected region."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (kill-ring-save (region-beginning) (region-end))
        text)
    (progn
      (message "No region selected")
      nil)))
;;;
(defvar night/h-interprogram-paste-from-file-last nil
  "The last string provided by `night/interprogram-paste-from-file'.")

(defun night/interprogram-paste-from-file ()
  "Reads the ~/.remote_clipboard file and returns its contents if it is different from the last paste."
  (when (file-exists-p "~/.remote_clipboard")
    (with-temp-buffer
      (insert-file-contents "~/.remote_clipboard")
      (let ((current-clip (buffer-string)))
        (unless (string= current-clip night/h-interprogram-paste-from-file-last)
          (setq night/h-interprogram-paste-from-file-last current-clip)
          ;; The return value of the setq form is the value of the last VAL.
          )))))

(when (night/ssh-p)
  (setq interprogram-paste-function 'night/interprogram-paste-from-file))
;;;

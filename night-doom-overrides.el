;;;
(setq
 ;; beware large gc thresholds: https://emacs.stackexchange.com/questions/5351/optimizing-font-lock-performance?rq=1
 ;; beware small thresholds as well -_-: emacs breaks as company continuously fills up its memory and gc throws them out
 ;;
 gcmh-idle-delay (cond
                           ((night/server-alt1-p) 300)
                           (t 15))
 gcmh-high-cons-threshold (cond
                           ((night/server-alt1-p) (* 512 1024 1024))
                           (t (* 32 1024 1024)))
 ;; gcmh-high-cons-threshold (* 512 1024 1024)
 ;; gcmh-high-cons-threshold (* 256 1024 1024)
 ;; the first number will be in megabytes. doom's default was 16mb
;;;
 ;; one of these two should be nil
 ;; gcmh-verbose t
 ;; garbage-collection-messages nil
 gcmh-verbose nil
 garbage-collection-messages t
;;;
 doom-modeline-window-width-limit 30    ;; @userConfig
 )
;;;
;; (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
;; * @upstreamBug? @hang [jalali:1402/02/08/18:07]
;; ** When this line was present, emacs hanged (by trying to do M-x or most other operations) when it was loaded from config.el. Evaling it later had no effect. This happened with emacs 28.2, 29.0.60, and 30.0.50.
;;
;; It seems this hook becomes empty after running?
;;
;; ** [[https://github.com/doomemacs/doomemacs/issues/7211][{REGRESSION} Weird bug that causes `remove-hook` to make emacs hang · Issue #7211 · doomemacs/doomemacs]]
;;
;; ** [jalali:1402/02/24/03:33] This doesn't work even on my laptop, so sth in doom has changed. It's a real bug.
;; *** [[file:~/.emacs.d/lisp/doom-editor.el::(use-package! smartparens][lisp/doom-editor.el::(use-package! smartparens]]
;;;
(remove-hook 'kill-emacs-hook #'recentf-cleanup)
;;;
(remove-hook 'text-mode-hook #'highlight-indent-guides-mode)
;;;
(defalias 'doom--update-files #'doom-files--update-refs)

(defun doom/move-this-file (new-path &optional force-p)
  "Move current buffer's file to NEW-PATH.

If FORCE-P, overwrite the destination file if it exists, without confirmation."
  (interactive
   (list (read-file-name "Move file to: ")
         current-prefix-arg))
  (unless (and buffer-file-name (file-exists-p buffer-file-name))
    (user-error "Buffer is not visiting any file"))
  (let* ((old-path (buffer-file-name (buffer-base-buffer)))
         (images-postfix "_imgs")
         (old-path-images (concat old-path images-postfix))
         (new-path (expand-file-name new-path)))

    (when (directory-name-p new-path)
      (setq new-path (concat new-path (f-filename old-path))))
    (make-directory (file-name-directory new-path) 't)
    (rename-file old-path new-path (or force-p 1))
    (set-visited-file-name new-path t t)

    (when (f-dir-p old-path-images)
      (let (;; (new-path-images (concat new-path images-postfix))
            ;; @todo0 to use the new filename as well, we need to first rename the image links in the current file.

            ;; just fix the dirname part, and continue using the old filename
            (new-path-images (concat (f-dirname new-path) "/" (f-filename old-path) images-postfix)))
        (when (not (f-equal-p old-path-images new-path-images))
          (rename-file old-path-images new-path-images (or force-p 1))
          (message "Also renamed the images directory:\n%s\n-> %s" old-path-images new-path-images)
          (z tts-glados1-cached "Also renamed the images directory")
          (doom--update-files old-path-images new-path-images ;; @redundant?
                              ))))

    (doom--update-files old-path new-path)

    (message "File moved to %S" (abbreviate-file-name new-path))))

(defun night/update-files (&rest files)
  (let ((org-files
         (-filter #'(lambda (file)
                      (equalp (f-ext file) "org")) files)))
    (when org-files
      (org-id-update-id-locations org-files t)))
  ;; (dolist (file files)
  ;;   )
  )

(advice-add #'doom--update-files :after #'night/update-files)

;;;
(after! org
  (defun night/+org--insert-item (direction &optional level prefix)
    (let (
          (prefix (or prefix ""))
          (context (org-element-lineage
                    (org-element-context)
                    '(table table-row headline inlinetask item plain-list)
                    t)))
      (pcase (org-element-type context)
        ;; Add a new list item (carrying over checkboxes if necessary)
        ((or `item `plain-list)
         ;; Position determines where org-insert-todo-heading and org-insert-item
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (org-end-of-item)
           (backward-char))

         (insert prefix)
         (org-insert-item (org-element-property :checkbox context))
         ;; Handle edge case where current item is empty and bottom of list is
         ;; flush against a new heading.
         (when (and (eq direction 'below)
                    (eq (org-element-property :contents-begin context)
                        (org-element-property :contents-end context)))
           (org-end-of-item)
           (org-end-of-line)))

        ;; Add a new table row
        ((or `table `table-row)
         (pcase direction
           ('below (save-excursion
                     (insert prefix)
                     (org-table-insert-row t))
                   (org-table-next-row))
           ('above (save-excursion
                     (insert prefix)
                     (org-shiftmetadown))
                   (+org/table-previous-row))))

        ;; Otherwise, add a new heading, carrying over any todo state, if
        ;; necessary.
        (_
         (let ((level (or level (org-current-level) 1)))
           ;; I intentionally avoid `org-insert-heading' and the like because they
           ;; impose unpredictable whitespace rules depending on the cursor
           ;; position. It's simpler to express this command's responsibility at a
           ;; lower level than work around all the quirks in org's API.
           (pcase direction
             (`below
              (let (org-insert-heading-respect-content)
                (goto-char (line-end-position))
                (org-end-of-subtree)
                (while (memq (preceding-char) '(?\n ?\^M))
                  ;; Go to end of line before heading
                  (forward-char -1)
                  )
                (insert prefix)
                (insert "\n" (make-string level ?*) " ")
                ))
             (`above
              (org-back-to-heading)
              (insert prefix)
              (insert (make-string level ?*) " ")
              (save-excursion (insert "\n"))))
           (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                       (todo-type    (org-element-property :todo-type context)))
             (org-todo
              (cond ((eq todo-type 'done)
                     ;; Doesn't make sense to create more "DONE" headings
                     (car (+org-get-todo-keywords-for todo-keyword)))
                    (todo-keyword)
                    ('todo)))))))

      (when (org-invisible-p)
        (org-show-hidden-entry))
      (when (and (bound-and-true-p evil-local-mode)
                 (not (evil-emacs-state-p)))
        (evil-insert 1)))))
;;;
(defun night/equal-str (a b)
  ;; (message "a: %s b: %s" a b)
  (equalp (format "%s" a) (format "%s" b))
  )

(setq +file-templates-alist
      (cl-remove 'emacs-lisp-mode +file-templates-alist :test 'night/equal-str :key 'car))
;; @note you need to restart emacs for this change to take effect
;;;
;; @userConfig
(defun night/overrides-doom-modeline-buffer-file-name ()
  "Propertized variable `buffer-file-name' based on `doom-modeline-buffer-file-name-style'."
  (let* ((buffer-file-name (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
         (buffer-file-truename (file-local-name
                                (or buffer-file-truename (file-truename buffer-file-name) "")))
         (file-name
          (pcase doom-modeline-buffer-file-name-style
            ('auto
             (if (doom-modeline-project-p)
                 (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil nil 'hide)
               (propertize "%b" 'face 'doom-modeline-buffer-file)))
            ('truncate-upto-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink))
            ('truncate-from-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil 'shrink))
            ('truncate-with-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink 'hide))
            ('truncate-except-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink))
            ('truncate-upto-root
             (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename))
            ('truncate-all
             (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename t))
            ('truncate-nil
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename))
            ('relative-to-project
             (doom-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename))
            ('relative-from-project
             (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil nil 'hide))
            ('file-name
             (propertize (file-name-nondirectory buffer-file-name)
                         'face 'doom-modeline-buffer-file))
            ((or 'buffer-name _)
             (propertize "%b" 'face 'doom-modeline-buffer-file))))
;;; @monkeyPatched
         (file-name (s-replace-regexp "^(?:cellar/)?notes/" "~nt/" file-name))
         ;; (file-name (s-replace-regexp "^doom.d/" "~dom/" file-name))
         (file-name
          (or
           (with-demoted-errors (let ((l (length file-name)))
                                  (cond
                                   ((> l 60)
                                    (concat
                                     (substring file-name 0 13)
                                     "ɣ"
                                     (substring file-name (- l 47) l)))
                                   (t file-name))))
           "error98128230124"))
;;;
         )
    (propertize (if (string-empty-p file-name)
                    (propertize "%b" 'face 'doom-modeline-buffer-file)
                  file-name)
                'mouse-face 'mode-line-highlight
                'help-echo (concat buffer-file-truename
                                   (unless (string= (file-name-nondirectory buffer-file-truename)
                                                    (buffer-name))
                                     (concat "\n" (buffer-name)))
                                   "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
                'local-map mode-line-buffer-identification-keymap)))

(advice-add 'doom-modeline-buffer-file-name :override #'night/overrides-doom-modeline-buffer-file-name)
;; This is ultimately where the modeline is constructed:
;; [[doom:.local/straight/repos/doom-modeline/doom-modeline-segments.el::(doom-modeline-def-segment buffer-info]]

(setq doom-modeline-buffer-file-name-style 'relative-from-project)
;;;
(defun night/trs (&rest paths)
  (z fnswap reval-ec reval trs (identity paths)))

(when (night/brish-p)
    (advice-add 'system-move-file-to-trash :override #'night/trs))
;;;
(defun night/counsel-jump-list ()       ;; @toFuture/1401 @upstreamMe
  "Go to an entry in evil's (or better-jumper's) jumplist."
  (interactive)
  ;; REVIEW Refactor me
  (let (buffers)
    (unwind-protect
        (ivy-read
         "jumplist: "
         (nreverse
          (delete-dups
           (delq
            nil
            (mapcar
             (lambda (mark)
               (when mark
                 (cl-destructuring-bind (path pt _id) mark
                   (when (f-exists-p path)
                     ;; @monkeyPatched to add the above check
                     (let ((buf (get-file-buffer path)))
                       (unless buf
                         (push (setq buf (find-file-noselect path t))
                               buffers))
                       (with-current-buffer buf
                         (goto-char pt)
                         (font-lock-fontify-region (line-beginning-position) (line-end-position))
                         (cons (format "%s:%d: %s"
                                       (buffer-name)
                                       (line-number-at-pos)
                                       (string-trim-right (or (thing-at-point 'line) "")))
                               (point-marker))))))))
             (cddr (better-jumper-jump-list-struct-ring
                    (better-jumper-get-jumps (better-jumper--get-current-context))))))))
         :sort nil
         :require-match t
         :action (lambda (cand)
                   (let ((mark (cdr cand)))
                     (delq! (marker-buffer mark) buffers)
                     (mapc #'kill-buffer buffers)
                     (setq buffers nil)
                     (with-current-buffer (switch-to-buffer (marker-buffer mark))
                       (goto-char (marker-position mark)))))
         :caller '+ivy/jump-list)
      (mapc #'kill-buffer buffers))))
(advice-add '+ivy/jump-list :override #'night/counsel-jump-list)
;; WAIT [[https://github.com/gilbertw1/better-jumper/issues/17][gilbertw1/better-jumper#17 Getting the jumplist sorted by recency]]
;;;

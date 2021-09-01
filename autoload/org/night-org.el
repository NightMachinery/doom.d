;;; Don't name this file org.el, emacs will think it's the actual org mode and things will break.
(after! org
  (require 'org-element)
;;;
  (add-hook 'org-mode-hook #'night/disable-company-frontends) ;; the tooltip frontend is buggy and moves the oorg headers indentations, which is visually distracting.
;;;
  ;; (setq org-cycle-emulate-tab 'exc-hl-bol) ; @weirdFeature
;;;
  (setq org-cycle-separator-lines 1)
;;;
  (setq org-return-follows-link t)
;;;
  (setq org-babel-min-lines-for-block-output 0) ; If number of lines of output is equal to or exceeds this value, the output is placed in a #+begin_example...#+end_exampleblock.
  (setq org-startup-with-inline-images t)
  ;; (setq org-src-tab-acts-natively nil) ; doesn't fix the completion TAB problem
  (defun night/org-save-hook-fn ()
    (interactive)
    (when (string= "org-mode" major-mode)
      ;; (org-html-export-to-html)
      (org-babel-tangle)
      ))
  (add-hook 'after-save-hook #'night/org-save-hook-fn)
;;;
  (setq org-image-actual-width '(700)) ; this zooms small images though and downscales big ones. It unfortunately overrides per-image attribute settings.
  ;; (setq org-image-actual-width '(fill-column))
;;;
  (defun night/org-paste-clipboard-image ()
    "Paste the image in the clipboard at point."
    (interactive)
    ;; (org-display-inline-images)
    (setq filename
          (concat
           (make-temp-name
            (concat (file-name-nondirectory (buffer-file-name))
                    "_imgs/"
                    (format-time-string "%Y%m%d_%H%M%S_"))) ".png"))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
                                        ; take screenshot
    (if (eq system-type 'darwin)
        ;; url-copy-file for downloading URLs
        ;; (call-process "pngpaste" nil nil nil filename)
        ;; @bug This always uses the png extension, while the file can be, e.g., jpg.
        (call-process "brishzq.zsh" nil nil nil "pbpaste-image" (concat (file-name-directory (buffer-file-name)) "/" filename))
      ;; (call-process "screencapture" nil nil nil "-i" filename)
      )
    (if (eq system-type 'gnu/linux)
        (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken

    (if (file-exists-p filename)
        (let* (
               (width-max 650) ;; 800, 850 are also possible, but big images slow emacs when scrolling
               (width-orig (string-to-number (z img-width (i filename))))
               (width-orig (/ width-orig 2.5))
               (width (cond
                       ((= width-orig 0) ;; parse error has happened
                        500)
                       ((<= width-orig width-max) width-orig)
                       (t width-max))))
          (insert (concat "#+ATTR_HTML: :width " (number-to-string (round width)) "\n[[file:" filename "]]\n"))))
    (org-redisplay-inline-images))

  (setq org-blank-before-new-entry '(
                                     (heading . t)
                                     ;; (heading . nil)
                                     ;; (heading . auto) ;; auto also adds newlines for subheadings which sucks
                                     (plain-list-item . t)
                                     ;; (plain-list-item . nil)
                                     ))
;;;
  (defun night/org-refile-to-new-file (&optional x)
    "Cut the subtree currently being edited and create a new file
from it.

If called with the universal argument, prompt for new filename,
otherwise use the subtree title."
    (interactive "P")
    (org-back-to-heading)
    (let* ((header-name (concat (org-element-property :title
                                                      (org-element-at-point)) ".org"))
           (filename (cond
                      (t ;; current-prefix-arg
                       (expand-file-name
                        (read-file-name "(@warn saves both files) New file name: " nil nil nil header-name)))
                      (t
                       (expand-file-name
                        header-name
                        default-directory)))))
      (org-cut-subtree)
      (save-buffer)
      (let ((new-buffer (find-file-noselect filename)))
        (with-current-buffer new-buffer
          (org-mode)
          (end-of-buffer)
          (or (bobp) (newline))
          (yank)
          (save-buffer))
        (switch-to-buffer new-buffer))
;;;
      ;; (find-file-noselect filename)
      ;; (with-temp-file filename
      ;;   (org-mode)
      ;;   (yank))
      ))
;;;
  )

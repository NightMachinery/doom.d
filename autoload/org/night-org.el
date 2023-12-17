;;; Don't name this file org.el, emacs will think it's the actual org mode and things will break.
(after! org
  (require 'org-element)
;;;
  (add-hook 'org-mode-hook #'night/disable-company-frontends) ;; the tooltip frontend is buggy and moves the org headers indentations, which is visually distracting.
;;;
  ;; (setq org-cycle-emulate-tab 'exc-hl-bol) ; @weirdFeature
;;;
  (setq org-cycle-separator-lines 1)
;;;
  (setq org-priority-highest 0)
  (setq org-priority-default 3)
  (setq org-priority-lowest 9)
  (setq org-priority-start-cycle-with-default nil)
;;;
  (setq org-return-follows-link t)
;;;
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
  (setq org-image-actual-width '(1000)) ; this zooms small images though and downscales big ones. It unfortunately overrides per-image attribute settings.
  ;; (setq org-image-actual-width '(fill-column))
;;;
  (defun night/org-paste-clipboard-image (&optional arg)
    "Paste the image in the clipboard at point.

@seeAlso [agfi:org-img-unused]"
    (interactive "P")
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
        (z "reval-to-stdout" "h-emc-paste-img"
           (concat (file-name-directory (buffer-file-name)) "/" filename)
           (if arg
               "y"
             "n"))
      ;; (call-process "screencapture" nil nil nil "-i" filename)
      )
    (if (eq system-type 'gnu/linux)
        (call-process "import" nil nil nil filename))
                                        ; insert into file if correctly taken

    (if (file-exists-p filename)
        (let* (
               (width-max 900) ;; 800, 850 are also possible, but big images slow emacs when scrolling
               (width-orig (string-to-number (z img-width (i filename))))
               (width-orig (/ width-orig 1.4))
               (width (cond
                       ((= width-orig 0) ;; parse error has happened
                        800)
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
(defun night/org-night-directive-present-p (directive)
  "Check if a given DIRECTIVE is present in the current org buffer, ignoring case."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (re-search-forward (format "^#[[:space:]]*NIGHT_DIRECTIVE:[[:space:]]*%s[[:space:]]*$" directive) nil t))))
  )
;;;
(defun night/org-go-to-last-heading (&optional level)
  "Go to the last heading in the current Org mode buffer."
  (interactive)
  (let* ((level (or level 1)))
    (goto-char (point-max)) ;; Go to the end of the buffer
    (unless (org-at-heading-p)
      (org-previous-visible-heading 1))
    (while (> (org-outline-level) level) ; While not at a first-level heading, go up.
      (org-up-heading-safe))))
;;;

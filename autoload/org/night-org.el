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
;;;
    (when (night/org-night-directive-present-p "night_autoexport_latex_to_pdf")
      (org-beamer-export-to-pdf))
;;;
    (when (string= "org-mode" major-mode)
      (night/highlight-background)
;;;
      ;; (org-html-export-to-html)
;;;
      ;; (org-babel-tangle)
;;;
      ))
;;;
  (setq org-image-actual-width '(800)) ; this zooms small images though and downscales big ones. It unfortunately overrides per-image attribute settings.
  ;; (setq org-image-actual-width '(fill-column))
;;;
  (defun night/h-org-generate-image-filename (format)
    "Generate a unique filename for the image with the given FORMAT."
    (concat
     (make-temp-name
      (concat (file-name-nondirectory (buffer-file-name))
              "_imgs/"
              (format-time-string "%Y%m%d_%H%M%S_"))) format))

(defun night/h-paste-image (filename transparent-p)
  "Capture the image and save it to FILENAME. Use TRANSPARENT-P to determine the capture method."
  (cond
   ((eq system-type 'darwin)
    (z "reval-to-stdout" "h-emc-paste-img"
       (concat (file-name-directory (buffer-file-name)) "/" filename)
       (if transparent-p "y" "n"))
    ;; take screenshot
    ;; (call-process "screencapture" nil nil nil "-i" filename)
    )
   ((eq system-type 'gnu/linux)
    (call-process "import" nil nil nil filename))))

(cl-defun night/org-insert-image
    (filename
     &key
     (max-height-before-split 1075)
     (max-height 800))
  "Insert the image at FILENAME into the org buffer. Split if necessary based on MAX-HEIGHT-BEFORE-SPLIT."
  (if (file-exists-p filename)
      (let* ((width-max 900)
             (height-orig (string-to-number (z img-height (i filename))))
             (width-orig (string-to-number (z img-width (i filename))))
             (width-orig (/ width-orig 1.4))
             (width (night/h-org-calculate-image-width width-orig width-max)))
        (if (< height-orig max-height-before-split)
            (night/org-insert-single-image filename width)
          (night/org-split-and-insert-image filename width max-height)))))

(defun night/h-org-calculate-image-width (width-orig width-max)
  "Calculate the width of the image based on WIDTH-ORIG and WIDTH-MAX."
  (cond
   ((= width-orig 0) 800) ;; parse error has happened
   ((<= width-orig width-max) width-orig)
   (t width-max)))

(defun night/org-insert-single-image (filename width)
  "Insert a single image at FILENAME with WIDTH into the org buffer."
  (insert
   (concat
    "#+ATTR_HTML: :width " (number-to-string (round width))
    "\n[[file:" filename "]]\n"))
  (org-redisplay-inline-images))

(cl-defun night/org-split-and-insert-image
    (filename
     width
     max-height
     &key
     (delete-original-p t)
     )
  "Split the image at FILENAME vertically and insert the parts with WIDTH into the org buffer."
  (message "Splitting images vertically ...")

  ;; `lexical-let' is necessary to capture these variables in the lambda.
  (lexical-let ((filename filename)
                (width width)
                (delete-original-p delete-original-p))
    (night/brishz-async-insert
     :name "night/org-split-and-insert-image"
     :command (list "img_vertical_split.py" filename "--max-height" max-height)
     ;; :callback-after #'night/nop
     :callback-after #'night/bell-lm-ok
     :insert-fn (lambda (filenames)
                  (let
                      ((filenames (split-string filenames "\n" t)))
                    (message "Images split to %s chunks." (length filenames))
                    (when delete-original-p
                      (night/trs filename))
                    (dolist (filename filenames)
                      (night/org-insert-single-image filename width))))
     :save-p t)))

  (defun night/org-paste-clipboard-image (&optional arg transparent-p format)
    "Paste the image in the clipboard at point.

@seeAlso [agfi:org-img-unused]"
    (interactive "P")
    ;; (org-display-inline-images)
    (let*
        ((split-p
          arg
          ;; (not arg)
          )
         (format
          (cond
           (transparent-p ".png") ;; `transparent-p' means remove background, so we need the alpha channel.
           (format format)
           (t ".jpg")))
         (filename (night/h-org-generate-image-filename format))
         (opts (unless split-p
                 (list :max-height-before-split 999999))))
      (night/mkdir-for-file filename)
      (night/h-paste-image filename transparent-p)
      (apply #'night/org-insert-image
             filename
             opts)))

  (defun night/org-paste-clipboard-image-png (&optional arg)
    (interactive "P")
    (night/org-paste-clipboard-image arg ".png"))

  (defun night/org-paste-clipboard-image-jpg (&optional arg)
    (interactive "P")
    (night/org-paste-clipboard-image arg ".jpg"))

  (defun night/org-paste-clipboard-image-transparent (&optional arg)
    (interactive "P")
    (night/org-paste-clipboard-image arg t))
;;;
  (setq org-blank-before-new-entry '(
                                     (heading . t)
                                     ;; (heading . nil)
                                     ;; (heading . auto) ;; auto also adds newlines for subheadings which sucks
                                     (plain-list-item . t)
                                     ;; (plain-list-item . nil)
                                     ))

  (defvar night/org-blank-after-new-heading-p t
    "If non-nil, insert a blank line after inserting a new heading in Org mode.")

  (defun night/insert-blank-after-org-heading (&rest dummy)
    "Insert a newline after a new org heading if `night/org-blank-after-new-heading-p` is true."
    (when night/org-blank-after-new-heading-p
      (save-excursion
        (end-of-line)
        (insert "\n"))))

  (advice-add 'org-insert-heading :after #'night/insert-blank-after-org-heading)
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
    (cond
     ((night/buffer-no-night-directives-p)
      nil
      ;; manually disable all directives for this file
      ;; useful for, e.g., chatting with LLMs where directives might be pasted from other files
      )
     (t (save-excursion
        (goto-char (point-min))
        (let ((case-fold-search t))
          (re-search-forward (format "^#[[:space:]]*NIGHT_DIRECTIVE:[[:space:]]*%s[[:space:]]*$" directive) nil t)))))))
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

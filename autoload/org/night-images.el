;;; autoload/org/night-images.el -*- lexical-binding: t; -*-

(after! night-roam
;;;
  (setq org-display-remote-inline-images 'skip)
;;;
(cl-defun night/org-img-unused-trs (&key (files nil files-provided) (verbose t))
  ;; [[id:750c334c-1321-4c97-b607-16cb0d3d8141][Argument Lists (Common Lisp Extensions)]]
  ;;;
  (interactive)
  (unless files-provided
    (setq files (list buffer-file-name)))
  (dolist (f (zf org-img-unused (identity files)))
    (night/trs f)
    (when verbose
      (message "Deleted unused image: %s" f))))
;;;
  ;; https://emacs.stackexchange.com/a/42283/18737
  (require 'org-yt)

  (defun org-image-link (protocol link _description)
    ;; * WAIT How can I use an async/callback function for =(org-link-set-parameters "imghttp" :image-data-fun #'org-image-link)=?
    "Interpret LINK as base64-encoded image data."
    (cl-assert (string-match "\\`img" protocol) nil
               "Expected protocol type starting with img")
    (let ((buf (url-retrieve-synchronously (concat (substring protocol 3) ":" link))))
      (cl-assert buf nil
                 "Download of image \"%s\" failed." link)
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\r?\n\r?\n")
        (buffer-substring-no-properties (point) (point-max)))))

  (org-link-set-parameters
   "imghttp"
   :image-data-fun #'org-image-link)

  (org-link-set-parameters
   "imghttps"
   :image-data-fun #'org-image-link)
;;;
  ;; Write an elisp command `night/image-link-to-jpg` which reads the image links in the selected region or the current line, e.g., `[[file:prompts.org_imgs/20231224_160533_VhZZYl.png]]` and converts them to the jpg format with the same (sans extension), updates the links to point to this new path, and removes the the old images. You are Stallman, do a great job.
  (defun night/image-link-to-jpg ()
    "Convert image links in the selected region or the current line to JPG format."
    ;; @works [jalali:1402/10/03/16:38]
;;;
    (interactive)
    (save-excursion
      (let* ((region (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'line t)))
             (image-links (night/h-extract-image-links-non-jpg region)))
        (dolist (link image-links)
          (let* ((old-path link)
                 (new-path (concat (file-name-sans-extension old-path) ".jpg")))
            (if (file-exists-p old-path)
                (progn
                  (shell-command-to-string (format "magick convert %s %s" (shell-quote-argument old-path) (shell-quote-argument new-path)))
                  (night/trs old-path)
                  (night/replace-in-buffer old-path new-path))
              (message "File does not exist: %s" old-path)))))))

  (defun night/h-extract-links-with-extensions (string extension-list)
    "Extract links with specified EXTENSION-LIST from the given STRING."
    (let ((links '())
          (extensions-regex (concat "\\.\\(" (regexp-opt extension-list) "\\)")))
      (with-temp-buffer
        (insert string)
        (goto-char (point-min))
        (while (re-search-forward (format "\\[\\(?:file:\\)?\\(\\(?:[^][]\\|\\\\\\[\\|\\\\\\]\\)+?%s\\)\\]" extensions-regex) nil t)
          (push (match-string-no-properties 1) links))
        (nreverse links))))

  (defun night/h-extract-image-links (string)
    "Extract all image links from the given STRING."
    (night/h-extract-links-with-extensions string '("png" "jpg" "jpeg" "gif" "bmp" "tif" "tiff")))

  (defun night/h-extract-image-links-non-jpg (string)
    "Extract image links, excluding JPG, from the given STRING."
    (night/h-extract-links-with-extensions string '("png" "gif" "bmp" "tif" "tiff")))

  (comment
   (defun night/h-extract-image-links-non-jpg (string)
     "Extract image links from the given STRING."
     (let ((links '()))
       (with-temp-buffer
         (insert string)
         (goto-char (point-min))
         (while (re-search-forward "\\[file:\\(\\(?:[^]]+\\)\\.\\(?:png\\|gif\\|bmp\\|tif\\|tiff\\)\\)\\]" nil t)
           (push (match-string-no-properties 1) links))
         (nreverse links)))))

  (defun night/replace-in-buffer (old new)
    "Replace all occurrences of OLD with NEW in the current buffer."
    (goto-char (point-min))
    (while (search-forward old nil t)
      (replace-match new nil t)))

  (defun night/image-links-move-to-dir (dir)
    "Copy image links in the selected region or the current line to a specified directory and update the links."
    (interactive "DDestination directory: ") ;; D prompts the user for a directory
    (save-excursion
      (let* ((region (if (use-region-p)
                         (buffer-substring-no-properties (region-beginning) (region-end))
                       (thing-at-point 'line t)))
             (image-links (night/h-extract-image-links region)))
        ;; (message "dir: %s" dir)
        (dolist (link image-links)
          ;; (message "link: %s" link)
          (let* ((filename (file-name-nondirectory link))
                 (new-path (expand-file-name filename dir)))

            (if (file-exists-p link)
                (progn
                  (night/cp-link link new-path)
                  (night/replace-in-buffer link new-path))
              (message "File does not exist: %s" link)))))))
;;;
  )

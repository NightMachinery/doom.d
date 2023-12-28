;;; autoload/org/night-images.el -*- lexical-binding: t; -*-

(after! night-roam
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

(defun night/h-extract-image-links-non-jpg (string)
  "Extract image links from the given STRING."
  (let ((links '()))
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward "\\[file:\\(\\(?:[^]]+\\)\\.\\(?:png\\|gif\\|bmp\\|tif\\|tiff\\)\\)\\]" nil t)
        (push (match-string-no-properties 1) links))
      (nreverse links))))

(defun night/replace-in-buffer (old new)
  "Replace all occurrences of OLD with NEW in the current buffer."
  (goto-char (point-min))
  (while (search-forward old nil t)
    (replace-match new nil t)))
;;;
  )

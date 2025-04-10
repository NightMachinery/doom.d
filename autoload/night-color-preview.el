;;; autoload/night-color-preview.el -*- lexical-binding: t; -*-

(defvar latex-color-regex
  "\\\\definecolor{\\([^}]+\\)}{RGB}{\\([0-9]+\\),\\s-*\\([0-9]+\\),\\s-*\\([0-9]+\\)}"
  "Regular expression to match LaTeX \\definecolor commands with RGB values.")

(cl-defun night/h-create-color-overlay (start end color &key (bg-p t))
  "Create an overlay from START to END with COLOR.
If BG-P is non-nil, set background, otherwise set foreground."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face `((t ,(if bg-p
                                       `(:background ,color)
                                     `(:foreground ,color)))))
    (overlay-put overlay 'night/color-preview-overlay t)
    overlay))

(defun night/h-color-preview-remove-color-overlays ()
  "Remove all color overlays created by night/latex-preview-colors functions."
  (interactive)
  (remove-overlays nil nil 'night/color-preview-overlay t))

(defun night/latex-preview-colors-bg (&optional buffer)
  "Add background color overlays for LaTeX color definitions in current or specified BUFFER."
  (interactive "P")
  (let ((verbosity-level 1)
        (target-buffer (if buffer
                          (read-buffer "Buffer: " (current-buffer))
                        (current-buffer))))
    (condition-case err
        (with-current-buffer target-buffer
          (night/h-color-preview-remove-color-overlays)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward latex-color-regex nil t)
              (let* ((color-name (match-string 1))
                     (r (string-to-number (match-string 2)))
                     (g (string-to-number (match-string 3)))
                     (b (string-to-number (match-string 4)))
                     (rgb-str (format "#%02X%02X%02X" r g b)))
                (message "Found color: %s at position %d-%d with RGB(%s,%s,%s) = %s"
                        color-name
                        (match-beginning 0) (match-end 0)
                        r g b rgb-str)
                (night/h-create-color-overlay
                 (match-beginning 0)
                 (match-end 0)
                 rgb-str
                 :bg-p t))))
          (message "Finished scanning buffer %s" (buffer-name target-buffer)))
      (error
       (message "Error in night/latex-preview-colors-bg: %s" err)
       nil))))

(defun night/latex-preview-colors-fg (&optional buffer)
  "Add foreground color overlays for LaTeX color definitions in current or specified BUFFER."
  (interactive "P")
  (let ((verbosity-level 1)
        (target-buffer (if buffer
                          (read-buffer "Buffer: " (current-buffer))
                        (current-buffer))))
    (condition-case err
        (with-current-buffer target-buffer
          (night/h-color-preview-remove-color-overlays)
          (save-excursion
            (goto-char (point-min))
            (while (re-search-forward latex-color-regex nil t)
              (let* ((color-name (match-string 1))
                     (r (string-to-number (match-string 2)))
                     (g (string-to-number (match-string 3)))
                     (b (string-to-number (match-string 4)))
                     (rgb-str (format "#%02X%02X%02X" r g b)))
                (message "Found color: %s at position %d-%d with RGB(%s,%s,%s) = %s"
                        color-name
                        (match-beginning 0) (match-end 0)
                        r g b rgb-str)
                (night/h-create-color-overlay
                 (match-beginning 0)
                 (match-end 0)
                 rgb-str
                 :bg-p nil))))
          (message "Finished scanning buffer %s" (buffer-name target-buffer)))
      (error
       (message "Error in night/latex-preview-colors-fg: %s" err)
       nil))))

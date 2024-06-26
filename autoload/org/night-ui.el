;;; autoload/org/night-ui.el -*- lexical-binding: nil; -*-

(after! (org night-color)
;;;
  ;; The following make =org-verse= look the same as =org-quote=.
  (custom-set-faces
   '(org-verse ((t (:inherit org-quote)))))
;;;
  (require 'jupyter-org-client)
  ;; to load =jupyter-org--ansi-color-apply-on-region=

  (defun night/babel-ansi1 ()
    (interactive)
    (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
      (save-excursion
        (goto-char beg)
        (when (looking-at org-babel-result-regexp)
          (let ((end (org-babel-result-end))
                (ansi-color-context-region nil))
            (ansi-color-apply-on-region beg end))))))

  (defun night/babel-ansi2 ()
    (interactive)
    (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
      (save-excursion
        (goto-char beg)
        (when (looking-at org-babel-result-regexp)
          (let* (
                 (end (org-babel-result-end)))


            (comment (when (search-forward "#+begin_example" nil t)
                       (replace-match "#+begin_results" nil t))
                     (while (search-forward "#+end_example" nil t))
                     (replace-match "#+end_results" nil t)
                     ;; I am now quoting the org stuff in ntt-org itself, as this solution somehow did not quote stuff completely
                     ;; update: it seems the problem is with the ANSI codes, our own quoter also doesn't work in its naive form
                     )

            (let* ((colored (xterm-color-filter (delete-and-extract-region beg end))))
              (goto-char beg)
              (insert colored)))))))

  (defun night/babel-ansi-all1 ()
    (interactive)
    ;; (save-mark-and-excursion
    ;;   (xterm-color-colorize-buffer))
    (let ((p (point)))
      ;; (message "p1: %s" p)
      (xterm-color-colorize-buffer)
      ;; (message "p2: %s" p)
      (goto-char p)))

  (defun night/babel-ansi-all2 ()
    (interactive)
    (let ((mod-p (buffer-modified-p)))
      (jupyter-org--ansi-color-apply-on-region (point-min) (point-max))
      (set-buffer-modified-p mod-p)
      ;; @assumes no concurrency can happen here
      ))

  (defalias 'night/babel-ansi #'night/babel-ansi2)
  (add-hook 'org-babel-after-execute-hook 'night/babel-ansi-all2)
  ;; (remove-hook 'org-babel-after-execute-hook 'night/babel-ansi-all2)
  ;; (remove-hook 'org-babel-after-execute-hook 'night/babel-ansi)
;;;
  ;; (set-face-attribute 'org-level-1 nil :box  `(:line-width 30 :color ,(face-background 'default)))

  (defun night/org-latex-preview-buffer ()
    (interactive)
    (org--latex-preview-region (point-min) (point-max)))

  (defun night/org-redisplay-images-etc ()
    (interactive)
    (night/org-latex-preview-buffer)
    (org-redisplay-inline-images)
    (night/babel-ansi-all2))

  (progn
    (face-spec-set 'org-level-5 ;; originally copied from org-level-8
                   (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
                     '((((class color) (min-colors 16) (background light)) (:foreground "royalblue"))
                       (((class color) (min-colors 16) (background dark)) (:foreground "royalblue"))
                       (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-6 ;; originally copied from org-level-8
                   (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
                     `((((class color) (min-colors 16) (background light)) (:foreground ,(night/color-256rgb-to-hex 0 109 109)))
                       (((class color) (min-colors 16) (background dark)) (:foreground ,(night/color-256rgb-to-hex 0 109 109)))
                       (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-7 ;; originally copied from org-level-8
                   (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
                     '((((class color) (min-colors 16) (background light)) (:foreground "deepskyblue4"))
                       (((class color) (min-colors 16) (background dark)) (:foreground "deepskyblue4"))
                       (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-8 ;; originally copied from org-level-8
                   (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
                     '((((class color) (min-colors 16) (background light)) (:foreground "Purple"))
                       (((class color) (min-colors 16) (background dark)) (:foreground "Purple"))
                       (((class color) (min-colors 8)) (:foreground "green")))))
    (defface org-level-9 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
          (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
          (((class color) (min-colors 8)) (:foreground "green"))))
      "Face used for level 9 headlines."
      :group 'org-faces)
    (setq org-level-faces (append org-level-faces (list 'org-level-9)))
    (setq org-n-level-faces (length org-level-faces))
    (cond
     ((display-graphic-p) ;; the current GUI theme doesn't differentiate between level 2 and 3 well, so I am just showing all the stars. It might be better this way anyway, even if the theme was fine.
      (setq org-hide-leading-stars nil
            org-indent-mode-turns-on-hiding-stars nil))
     (t
      ;; We don't want to hide the leading stars ever, period. :D
      (setq org-hide-leading-stars nil
            org-indent-mode-turns-on-hiding-stars nil)))))

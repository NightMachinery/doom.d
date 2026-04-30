;;; autoload/org/babel/night-org-babel-comments.el -*- lexical-binding: t; -*-

(after! org
  (require 'org-src)

  (defun night/org-example-block-language (element)
    "Return language tag for Org example-block ELEMENT, or nil.

This supports blocks written as `#+begin_example LANG'.  Org does not
record a language property for example blocks, so we parse the block
header directly."
    (when (and element (org-element-type-p element 'example-block))
      (save-excursion
        (goto-char (org-element-property :begin element))
        (let ((case-fold-search t))
          (when (looking-at "^[[:space:]]*#\\+begin_example[[:space:]]+\\([^[:space:]]+\\)")
            (match-string-no-properties 1))))))

  (defun night/org-block-body-region (element)
    "Return `(BEG END)' for ELEMENT's body, excluding Org block markers."
    (when-let* ((begin (org-element-property :begin element))
                (end (org-element-property :end element)))
      (save-excursion
        (goto-char begin)
        (forward-line 1)
        (let ((body-beg (point)))
          (goto-char end)
          (let ((case-fold-search t))
            (when (re-search-backward "^[[:space:]]*#\\+end_\\(?:src\\|example\\)" begin t)
              (list body-beg (line-beginning-position))))))))

  (defun night/org-region-strictly-in-region-p (beg end outer-beg outer-end)
    "Return non-nil when BEG..END is inside OUTER-BEG..OUTER-END."
    (and (<= outer-beg beg)
         (<= end outer-end)))

  (defun night/org-comment-region-as-language (beg end lang)
    "Comment or uncomment BEG..END as LANG, replacing text in the Org buffer.

Return non-nil when LANG resolved to an available major mode."
    (let* ((mode (and lang (org-src-get-lang-mode lang))))
      (when (and mode (fboundp mode))
        (let* ((old-pos (point))
               (text (buffer-substring-no-properties beg end))
               new-text)
          (with-temp-buffer
            (insert text)
            (funcall mode)
            (comment-or-uncomment-region (point-min) (point-max))
            (setq new-text (buffer-substring-no-properties (point-min) (point-max))))
          (delete-region beg end)
          (goto-char beg)
          (insert new-text)
          (goto-char (min old-pos (point-max)))
          t))))

  (defun night/org-comment-example-block-region (beg end)
    "Comment or uncomment BEG..END as an example block's language.

Return non-nil when the region was handled."
    (let* ((element (save-excursion
                    (goto-char beg)
                    (org-element-at-point)))
           (lang (night/org-example-block-language element))
           (body (and lang (night/org-block-body-region element))))
      (and body
           (night/org-region-strictly-in-region-p beg end (nth 0 body) (nth 1 body))
           (night/org-comment-region-as-language beg end lang))))

  (defun night/org-comment-or-uncomment-region-a (old-fn beg end &rest args)
    "Like OLD-FN, but use language syntax in tagged example blocks."
    (if (night/org-comment-example-block-region beg end)
        nil
      (apply old-fn beg end args)))

  (unless (advice-member-p #'night/org-comment-or-uncomment-region-a
                            #'org-comment-or-uncomment-region)
    (advice-add #'org-comment-or-uncomment-region
                :around #'night/org-comment-or-uncomment-region-a)))

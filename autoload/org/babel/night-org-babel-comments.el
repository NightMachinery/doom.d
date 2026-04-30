;;; autoload/org/babel/night-org-babel-comments.el -*- lexical-binding: t; -*-

(after! org
  (require 'org-src)

  (defun night/org-example-block-context-at (pos)
    "Return language and body bounds for a tagged example block at POS.

The return value is `(LANG BODY-BEG BODY-END)'.  This intentionally uses
nearby block-marker regexps instead of `org-element-at-point', because huge
example blocks can make the parser path noticeably slow during `gcc'."
    (save-excursion
      (goto-char pos)
      (let ((case-fold-search t)
            lang body-beg body-end)
        (when (re-search-backward "^[[:space:]]*#\\+begin_example[[:space:]]+\\([^[:space:]]+\\)" nil t)
          (setq lang (match-string-no-properties 1))
          (forward-line 1)
          (setq body-beg (point))
          (when (and (<= body-beg pos)
                     (re-search-forward "^[[:space:]]*#\\+end_example" nil t))
            (setq body-end (match-beginning 0))
            (when (< pos body-end)
              (list lang body-beg body-end)))))))

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
    (when-let* ((context (night/org-example-block-context-at beg))
                (lang (nth 0 context))
                (body-beg (nth 1 context))
                (body-end (nth 2 context)))
      (and (night/org-region-strictly-in-region-p beg end body-beg body-end)
           (night/org-comment-region-as-language beg end lang))))

  (defun night/org-comment-or-uncomment-region-a (old-fn beg end &rest args)
    "Like OLD-FN, but use language syntax in tagged example blocks."
    (if (night/org-comment-example-block-region beg end)
        nil
      (apply old-fn beg end args)))

  (unless (advice-member-p #'night/org-comment-or-uncomment-region-a
                            #'org-comment-or-uncomment-region)
    (advice-add #'org-comment-or-uncomment-region
                :around #'night/org-comment-or-uncomment-region-a))

  (after! evil-nerd-commenter
    (defun night/evilnc-comment-or-uncomment-region (start end)
      "Comment START..END, delegating Org buffers to Org's shared comment path.

This avoids Evil Nerd Commenter's slow Org parser path in huge blocks while
preserving EvilNC's normal behavior outside Org."
      (if (derived-mode-p 'org-mode)
          (comment-or-uncomment-region start end)
        (evilnc-comment-or-uncomment-region-internal start end)))

    (setq evilnc-comment-or-uncomment-region-function
          #'night/evilnc-comment-or-uncomment-region)))

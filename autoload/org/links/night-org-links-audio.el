;;; autoload/org/links/night-org-links-audio.el -*- lexical-binding: nil; -*-

(after! (org ol night-org-zshfile)
  (org-link-set-parameters "audiofile" :follow #'night/org-link-zshfile-follow)
  ;;;
  (cl-defun night/audiofile-link-get-current-line (&key (open-link-p nil))
    "Check if the current line contains an 'audiofile' link and return the path.

If OPEN-LINK-P is non-nil, open the link using Org functions."

    (interactive)
    (save-excursion

      (let* ((line-text (night/current-line-get))
             (link-regex "\\[audiofile:\\(\\(?:\\\\\\]\\|[^]]\\)+\\)\\]"))
        (when (string-match link-regex line-text)
          (let ((link-path (match-string 1 line-text)))
            (when open-link-p
              (org-open-link-from-string (format "[[audiofile:%s]]" link-path)))
            link-path)))))
;;;

  (cl-defun night/org-link-subtree-gather (&key (depth nil) (exclude-tags '("skip")) (region-p t))
    "Collect all links in the current subtree or selected region as a list of (cons type path).

Skip links with tags specified in EXCLUDE-TAGS (default: '(skip)).

The optional keyword argument :depth controls the recursion depth.

When REGION-P is non-nil and a region is selected, gather links from the selected region only.

When called interactively, display the found links in a message."
    ;; @dynamicBinding The closure here dynamically mutates `links'.
    (interactive)
    (save-excursion
      (let ((links '())
            (collect-links
             (lambda ()
               (let ((ast (org-element-parse-buffer)))
                 (org-element-map ast 'link
                   (lambda (link)
                     (let ((link-type (org-element-property :type link))

                           (link-tags (org-element-property :tags (org-element-lineage link '(headline) t)))
                           (link-path (org-element-property :path link)))
                       (unless (seq-intersection link-tags exclude-tags)
                         (push (cons link-type link-path) links))))
                   nil nil depth)))))
        ;; Determine the scope based on region-p and whether a region is selected
        (if (and region-p (use-region-p))
            (save-restriction
              ;; Narrow to the selected region
              (narrow-to-region (region-beginning) (region-end))
              ;; Collect links from the region

              (funcall collect-links))
          ;; Else, work with the entire subtree
          (org-back-to-heading t)
          (save-restriction
            (org-narrow-to-subtree)
            ;; Collect links from the subtree
            (funcall collect-links)))
        (setq links (seq-reverse links))
        ;; If called interactively, display the links
        (when (called-interactively-p 'any)
          (if links
              (message "Found links:\n\t%s" (mapconcat (lambda (link) (format "%s:%s" (car link) (cdr link))) links "\n\t"))
            (message "No links found.")))
        links)))

  (defun night/org-subtree-play-as-playlist ()
    "Create a temporary playlist from audio file links in the current subtree and play it using `night/hear`."
    (interactive)
    (let* ((links (night/org-link-subtree-gather))
           (audio-links (cl-remove-if-not (lambda (link) (string= (car link) "audiofile")) links))
           (audio-paths (mapcar (lambda (link) (night/path-unabbrev (cdr link))) audio-links))
           (temp-file
            (make-temp-file
             "org-subtree-playlist"
             nil
             ".raw_playlist"
             ;; ".m3u"
             )))
      (if audio-paths
          (progn
            (with-temp-file temp-file
              (insert (mapconcat #'identity audio-paths "\n")))
            (night/hear temp-file))
        (message "No audio file links found in the current subtree."))))
;;;
  )

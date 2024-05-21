;;; night-org-id-links.el ---                        -*- lexical-binding: t; -*-
;;;
;; Most of the logic is in [[./night-org-links-grep.el]].
;;;
(defun night/org-id-path-get (id)
  (let*
      ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))

    (buffer-file-name (marker-buffer m))))

(defun night/org-id-line-get (id)
  (let* ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))
    (with-current-buffer (marker-buffer m)
      (save-excursion
        (goto-char m)
        (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
(comment
 (org-id-find "9fe5919e-28d6-4a5f-a592-ae91f99fc169" 'marker)
 (night/org-id-path-get "9fe5919e-28d6-4a5f-a592-ae91f99fc169")
 (night/org-id-line-get "9fe5919e-28d6-4a5f-a592-ae91f99fc169")
 (night/org-id-path-get "61d2cdd6-23fd-4e31-a27b-1a1c61759be4")
 (night/org-id-line-get "61d2cdd6-23fd-4e31-a27b-1a1c61759be4"))
;;;
(provide 'night-org-id-links)
;;; night-org-id-links.el ends here

;;; night-org-id-links.el ---                        -*- lexical-binding: t; -*-
;;;
(defun night/org-id-path-get (id)
  (let*
      ((m (org-id-find id 'marker)))
    (unless m
      (error "Cannot find entry with ID \"%s\"" id))

    (buffer-file-name (marker-buffer m))))

(comment
 (night/org-id-path-get "61d2cdd6-23fd-4e31-a27b-1a1c61759be4"))
;;;
(provide 'night-org-id-links)
;;; night-org-id-links.el ends here

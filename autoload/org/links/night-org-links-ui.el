;;; autoload/org/night-org-links-ui.el -*- lexical-binding: t; -*-
;;;
(after! (org ol)
;; (org-link-set-parameters "nightNotes" :face 'org-link-nightNotes)
;; these abbreviation link types are converted to a file link, and org-activate-links sees them as having the type "file".

;; (defface night/org-link-files '((t (:foreground "sienna4" :background "cornsilk" :weight bold))) "face for nightNotes links")
;; (org-link-set-parameters "file" :face 'night/org-link-files)
;;;
  (defun org-activate-links (limit)
    "Add link properties to links.
This includes angle, plain, and bracket links."
    (catch :exit
      (while (re-search-forward org-link-any-re limit t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (visible-start (or (match-beginning 3) (match-beginning 2)))
               (visible-end (or (match-end 3) (match-end 2)))
               (style (cond ((eq ?< (char-after start)) 'angle)
                            ((eq ?\[ (char-after (1+ start))) 'bracket)
                            (t 'plain))))
          (when (and (memq style org-highlight-links)
                     ;; Do not span over paragraph boundaries.
                     (not (string-match-p org-element-paragraph-separate
                                          (match-string 0)))
                     ;; Do not confuse plain links with tags.
                     (not (and (eq style 'plain)
                               (let ((face (get-text-property
                                            (max (1- start) (point-min)) 'face)))
                                 (if (consp face) (memq 'org-tag face)
                                   (eq 'org-tag face))))))
            (let* ((link-object (save-excursion
                                  (goto-char start)
                                  (save-match-data (org-element-link-parser))))
                   (link (org-element-property :raw-link link-object))
                   (type (org-element-property :type link-object))
                   (path (org-element-property :path link-object))
                   (face-property (pcase (org-link-get-parameter type :face)
                                    ((and (pred functionp) face) (funcall face path))
                                    ((and (pred facep) face) face)
                                    ((and (pred consp) face) face) ;anonymous
                                    (_ 'org-link)))
                   (properties          ;for link's visible part
                    (list 'mouse-face (or (org-link-get-parameter type :mouse-face)
                                          'highlight)
                          'keymap (or (org-link-get-parameter type :keymap)
                                      org-mouse-map)
                          'help-echo (pcase (org-link-get-parameter type :help-echo)
                                       ((and (pred stringp) echo) echo)
                                       ((and (pred functionp) echo) echo)
                                       (_ (concat "LINK: " link)))
                          'htmlize-link (pcase (org-link-get-parameter type
                                                                       :htmlize-link)
                                          ((and (pred functionp) f) (funcall f))
                                          (_ `(:uri ,link)))
                          'font-lock-multiline t)))

              (org-remove-flyspell-overlays-in start end)
              (org-rear-nonsticky-at end)
              (if (not (eq 'bracket style))
                  (progn
                    (add-face-text-property start end face-property)
                    (add-text-properties start end properties))
                ;; Handle invisible parts in bracket links.
                (remove-text-properties start end '(invisible nil))
                (let ((hidden
                       (append `(invisible
                                 ,(or (org-link-get-parameter type :display)
                                      'org-link))
                               properties)))
                  (add-text-properties start visible-start hidden)

;;; @monkeyPatched
                  ;; (message "type: %s, face: %s" type face-property)

                  ;; (add-face-text-property start end face-property)
                  (add-face-text-property visible-start visible-end face-property)
;;;

                  (add-text-properties visible-start visible-end properties)
                  (add-text-properties visible-end end hidden)
                  (org-rear-nonsticky-at visible-start)
                  (org-rear-nonsticky-at visible-end)))
              (let ((f (org-link-get-parameter type :activate-func)))
                (when (functionp f)
                  (funcall f start end path (eq style 'bracket))))
              (throw :exit t)))))       ;signal success
      nil)))
;;;

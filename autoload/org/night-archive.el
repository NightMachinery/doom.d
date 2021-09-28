;;; autoload/org/night-archive.el -*- lexical-binding: t; -*-

;;;
(defun night/org-archive-done-tasks ()
  ;; https://stackoverflow.com/questions/6997387/how-to-archive-all-the-done-tasks-using-a-single-command
  ;; @alt https://emacs.stackexchange.com/a/21952/18737
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE" 'file)
  ;; change 'tree to 'file to operate over the entire file, and viceversa to operate only on current subtree
  )
;;;
;; See also [[nightNotes:cheatsheets/emacs/orgmode/archiving.org]]
;; Copied from:
;; https://github.com/Fuco1/.emacs.d/blob/500cccb4d5bbe8c2c3a1438b9c960938117e7105/files/org-defs.el
;; https://emacs.stackexchange.com/a/49084/18737
(defun org-get-local-archive-location ()
  "Get the archive location applicable at point."
  (let ((re "^[ \t]*#\\+ARCHIVE:[ \t]+\\(\\S-.*\\S-\\)[ \t]*$")
    prop)
    (save-excursion
      (save-restriction
    (widen)
    (setq prop (org-entry-get nil "ARCHIVE" 'inherit))
    (cond
     ((and prop (string-match "\\S-" prop))
      prop)
     ((or (re-search-backward re nil t)
          (re-search-forward re nil t))
      (match-string 1))
     (t org-archive-location))))))

(defun org-extract-archive-file (&optional location)
  "Extract and expand the file name from archive LOCATION.
if LOCATION is not given, the value of `org-archive-location' is used."
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (if (= (match-beginning 1) (match-end 1))
      (buffer-file-name (buffer-base-buffer))
    (expand-file-name
     (format (match-string 1 location)
         (file-name-nondirectory
          (buffer-file-name (buffer-base-buffer))))))))

(defun org-extract-archive-heading (&optional location)
  "Extract the heading from archive LOCATION.
if LOCATION is not given, the value of `org-archive-location' is used."
  (setq location (or location org-archive-location))
  (if (string-match "\\(.*\\)::\\(.*\\)" location)
      (format (match-string 2 location)
              (file-name-nondirectory
               (buffer-file-name (buffer-base-buffer))))))

(defadvice org-archive-subtree (around fix-hierarchy activate)
  (let* ((fix-archive-p (and (not current-prefix-arg)
                             (not (use-region-p))))
         (afile (org-extract-archive-file (org-get-local-archive-location)))
         (buffer (or (find-buffer-visiting afile) (find-file-noselect afile)))
         ;; Get all the parents and their tags, we will try to
         ;; recreate the same situation in the archive buffer.
         ;; TODO: make this customizable.
         (parents-and-tags (save-excursion
                             (let (parents)
                               (while (org-up-heading-safe)
                                 (push (list :heading (org-get-heading t t t t)
                                             :tags (org-get-tags nil :local))
                                       parents))
                               parents))))
    ad-do-it
    (when fix-archive-p
      (with-current-buffer buffer
        (goto-char (point-max))
        (while (org-up-heading-safe))
        (let* ((olpath (org-entry-get (point) "ARCHIVE_OLPATH"))
               (path (and olpath
                          (--map
                           (replace-regexp-in-string "^/" "" it)
                           (s-slice-at "/\\sw" olpath))))
               (level 1)
               tree-text)
          (when olpath
            (org-mark-subtree)
            (setq tree-text (buffer-substring (region-beginning) (region-end)))
            (let (this-command) (org-cut-subtree))
            (goto-char (point-min))
            (save-restriction
              (widen)
              (-each path
                (lambda (heading)
                  (if (re-search-forward
                       (rx-to-string
                        `(: bol (repeat ,level "*") (1+ " ") ,heading)) nil t)
                      (progn
                        (org-narrow-to-subtree)
                        (org-set-tags (plist-get (car parents-and-tags) :tags)))
                    (goto-char (point-max))
                    (unless (looking-at "^")
                      (insert "\n"))
                    (insert (make-string level ?*)
                            " "
                            heading)
                    (org-set-tags (plist-get (car parents-and-tags) :tags))
                    (end-of-line)
                    (insert "\n"))
                  (pop parents-and-tags)
                  (cl-incf level)))
              (widen)
              (org-end-of-subtree t t)
              (org-paste-subtree level tree-text)))))))
  (save-some-buffers))
;;;

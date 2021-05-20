;;; night-dash.el ---                                -*- lexical-binding: nil; -*-

;;;
(defun night/counsel-dash ()
  (interactive)
  (let (
        (fp night/fullscreen-popups)

        ;; @upstreamBug this dash-docs-common-docsets doesn't seem to work, so I am overriding dash-docs-docsets directly
        (dash-docs-docsets (-concat (when (boundp 'dash-docs-common-docsets) dash-docs-common-docsets) (when (boundp 'dash-docs-docsets) dash-docs-docsets) '("CSS" "HTML" "Python 3")))
        )
    (unwind-protect
        (progn (night/fullscreen-popups-enable)
               (counsel-dash))
      (when (not fp)
        (night/fullscreen-popups-disable)))))

(map!
 :leader
 "h h" #'night/counsel-dash)
;;;
(defun night/html-viewer (file)
  ;; (night/brishz  "emc-html-viewer" file)
  (let ((bn "*night-html-viewer*"))
    (ignore-errors (kill-buffer bn))
    (let*
        ((b (get-buffer-create bn)))
      (with-current-buffer b
        (org-mode)
        (insert (z html2org (z file-uri2unix (z trimr-hash (i file)))))
        (goto-char (point-min))
        )
      (pop-to-buffer b))))

(after! dash-docs
  ;; (setq dash-docs-browser-func #'eww)
  ;; (setq dash-docs-browser-func #'night/html-viewer)
  )
;;;

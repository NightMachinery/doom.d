;;; autoload/org/night-hider.el -*- lexical-binding: t; -*-

(after! (org evil-org)
  (defun night/org-heading-region-get (&optional position)
    (interactive)
    (let ((position (or position (point))))
      (save-excursion
        (goto-char position)
        (let ((start
               (progn
                 (org-back-to-heading-or-point-min)
                 (point)))
              (end
               (progn
                 (org-next-visible-heading 1)
                 (point))))
          (list start end)))))
  (defun night/org-heading-hide (&optional position)
    (interactive)
    (let* ((position (or position (point)))
           (r (night/org-heading-region-get))
           (start (car r))
           (end (cadr r)))

      (with-buffer-modified-unmodified
;;;
       ;; When I run `(add-text-properties 1 100 '(invisible t))`, it has no effects. Using `(add-text-properties 1 100 '(display ""))` works. Any ideas what I am doing wrong? `buffer-invisibility-spec does contain `t`.
       ;; It's because of org-mode! It removes the properties or sth.
       ;; (remove-text-properties start end '(invisible nil))
       ;; (add-text-properties start end `(invisible 'night/org-playlist))
;;;
       ;; (add-text-properties start end `(display ""))
;;;
       (outline-flag-region start (- end 1) t))
      (org-next-visible-heading 1)))

  (comment
   (defun night/org-playlist-hide ()
     (interactive)
     (add-to-invisibility-spec '(night/org-playlist))
     (org-restart-font-lock))
   (defun night/org-playlist-show ()
     (interactive)
     (remove-from-invisibility-spec '(night/org-playlist))
     (org-restart-font-lock)))


  (defun night/text-prop-display-remove-all ()
    (interactive)
    (with-buffer-modified-unmodified
     (remove-text-properties
      (point-min) (point-max)
      `(display ""))))

  (defun night/org-playlist-cp-to-tmp ()
    (interactive)
    (let* ((tmp-dir
            (night/ensure-dir (night/path-unabbrev "~tmp/playlists/")))
           (tmp (make-temp-file tmp-dir nil
                                (concat (ntag "org-playlist") ".org"))))
      (write-file tmp)))

  (defun night/org-playlist-hotkeys-enable ()
    (interactive)
    (map! :map 'local
          :localleader
          "pp"
          #'night/org-subtree-play-as-playlist

          ;; "\\\\"
          "pc"
          #'night/org-playlist-cp-to-tmp
          ;; "\\p"
          "pC"
          #'night/org-todo-copy
          )
    (comment
     (map! :map 'local
           :nv
           "x" #'night/org-heading-hide
           :nv
           "X" #'outline-show-all
           ;; "X" #'night/text-prop-display-remove-all
           :v
           "y" #'org-copy-visible
           ))))

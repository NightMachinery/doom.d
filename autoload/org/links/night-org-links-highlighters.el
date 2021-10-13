;;; autoload/org/links/night-org-links-highlighter.el -*- lexical-binding: t; -*-

(after! (org evil-org ol)
  (defvar night/org-link-re-highlighter "\\[\\[highlight:\\(\\(?:[^][]\\|\\\\\\[\\|\\\\\\]\\)*\\)\\]\\[[^][]*\\]\\]")

  (defun night/org-link-highlighter-get-str-backward ()
    (interactive)
    (save-excursion
      (re-search-forward "\\]\\]") ;; go to after the current link
      (let* (;; (end (- (point) 0))
             (start (progn
                      (re-search-backward night/org-link-re-highlighter)
                      (point)))
             (end (match-end 0)))
        (buffer-substring-no-properties start end))))

  (defun night/org-link-highlighter-backward ()
    (interactive)
    (re-search-backward night/org-link-re-highlighter)
    (message "Link: %s"
             (buffer-substring-no-properties
              (+ 14 (match-beginning 0))
              (+ 1 (match-beginning 1))
              ;; @idk why (match-beginning 1) is set so wrongly
              ))
    (comment (message "Link: %s, %s, %s"
              (match-beginning 1)
              (match-end 1)
              (buffer-substring-no-properties
               (match-beginning 1)
               (match-end 0))
              ))
    (cond
     (t
      (re-search-forward "\\]\\["))
     (t
      (dotimes (i 1)
        (night/point-increment))))
    (night/org-reveal-maybe)
    (night/screen-center))
  (defun night/org-link-highlighter-forward ()
    (interactive)
    (re-search-forward night/org-link-re-highlighter)
    (cond
     (t (night/org-link-highlighter-backward)) ;; end up at the start of the match
     (t (night/point-decrement)))
    (night/screen-center))

  (defun night/org-link-highlighter-open-at-point (&optional position)
    (interactive)
    (let* ((position (or position (point)))
           (link-str
            (with-demoted-errors (night/org-link-highlighter-get-str-backward))))
      (cond
       (link-str
        (org-link-open-from-string link-str))
       (t
        t)
       (t
        ;; this can't work, as we jump to the previous highlighter link without checking to see if we are in one at all. So If it fails, it means that we are at about the beginning of the buffer.
        (save-excursion
          (goto-char position)
          (org-open-at-point-global))))))

  (defun night/org-link-highlighter-hotkeys-enable ()
    ;; the tag =..org-highlighter..= will call us
    (interactive)
    (map! :map 'local
          :nvo
          "J" #'night/org-link-highlighter-forward
          :nvo
          "K" #'night/org-link-highlighter-backward
          :nvo
          "O" #'night/org-link-highlighter-open-at-point
          ))

  (defun night/org-link-highlighter-follow (path arg)
    ;; (message "night/org-link-highlighter-follow: not implemented yet; path: %s, arg: %s" path arg)
    (let* ((opts (s-split "," path))
           (path (cddr opts) ;; returns nil if length insufficient
                 )
           (path (night/empty-str-to-nil (s-join "," path))))
      (when path
        (org-link-open-from-string
         (concat "[[" (org-link-escape path) "]]"))
        (night/screen-center))))

  (org-link-set-parameters "highlight" :follow #'night/org-link-highlighter-follow)

  ;; also looks good: orangered,snow
  ;; @dup note that `org-activate-links' also has its own defaults for empty strings supplied for the colors
  (defface night/org-link-face-highlighter '((t (:foreground "green"
                                                 :background "ghostwhite"
                                                 :weight bold))) "face for highlighter links")
  (org-link-set-parameters "highlight" :face 'night/org-link-face-highlighter)

  (org-link-set-parameters "highlight" :display 'night/org-link-highlighter)
  (defun night/org-link-highlighter-hide ()
    (interactive)
    (add-to-invisibility-spec '(night/org-link-highlighter))
    (org-restart-font-lock))
  (defun night/org-link-highlighter-show ()
    (interactive)
    (remove-from-invisibility-spec '(night/org-link-highlighter))
    (org-restart-font-lock))
  )

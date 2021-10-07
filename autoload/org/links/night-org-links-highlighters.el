;;; autoload/org/links/night-org-links-highlighter.el -*- lexical-binding: t; -*-

(after! (org evil-org ol)
  (defvar night/org-link-re-highlighter "\\[\\[highlight:\\([^][]\\|\\\\\\[\\|\\\\\\]\\)*\\]\\[[^][]*\\]\\]")

  (defun night/org-link-highlighter-backward ()
    (interactive)
    (re-search-backward night/org-link-re-highlighter)
    (night/point-increment)
    (night/screen-center))
  (defun night/org-link-highlighter-forward ()
    (interactive)
    (re-search-forward night/org-link-re-highlighter)
    (night/point-decrement)
    (night/screen-center))

  (defun night/org-link-highlighter-hotkeys-enable ()
    ;; the tag =..org-highlighter..= will activate the scrollback-mode, which contains these keybindings
    (interactive)
    (map! :map evil-org-mode-map
          ;;; @duplicateCode/a31610096f2c63be7ae6d9e3736f7dfc
          :nvo
          "J" #'night/org-link-highlighter-forward
          :nvo
          "K"i #'night/org-link-highlighter-backward
          ;;;
          ))

  (defun night/org-link-highlighter-follow (path arg)
    ;; (message "night/org-link-highlighter-follow: not implemented yet; path: %s, arg: %s" path arg)
    (let* ((opts (s-split "," path))
           (path (nth 2 opts) ;; returns nil if length insufficient
                 ))
      (org-link-open-from-string
       (concat "[[" (org-link-escape path) "]]"))
      (night/screen-center)))

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

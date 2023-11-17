;;; autoload/org/links/night-agfi.el -*- lexical-binding: t; -*-

(after! (org ol)
  (defun night/org-link-agfi-follow (path arg)
    ;; [jalali:1402/01/26/15:56] =awaysh kitty-remote focus-tab --match="id:${i}"= does not work. We can fix this by using =brishzq.zsh awaysh= but I hope this goes away on its own.
    ;; (message "night/org-link-agfi-follow: %s" path)
    (z awaysh agfi-ni (i path)))
  (org-link-set-parameters "agfi" :follow #'night/org-link-agfi-follow)

  (defface night/org-link-face-agfi '((t (:foreground "blue4"
                                          ;; :background "azure"
                                          :weight bold))) "face for nightNotes links")
  (org-link-set-parameters "agfi" :face 'night/org-link-face-agfi)

  )

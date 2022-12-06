;;; night-slideshow.el ---                           -*- lexical-binding: nil; -*-
;;;
(require 'org-tree-slide)
(after! org-tree-slide
  ;; @seeAlso [help:night/screen-center-ni]
;;;
  ;; I don't know why [help:hide-subtree] does't work for us.
  (defalias 'hide-subtree #'org-fold-hide-subtree)
  (defalias 'show-subtree #'org-fold-show-subtree)

  (setq org-tree-slide-fold-subtrees-skipped nil)
  (setq org-tree-slide-skip-outline-level 3)
  ;; @seeAlso [help:night/org-tree-slide-skip-outline-level-for-going-back]

  (setq +org-present-text-scale 0)

  (setq org-tree-slide-header nil)
  (setq org-tree-slide-slide-in-effect nil)
  (setq org-tree-slide-cursor-init nil)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-skip-comments t)

  ;; The modeline is hidden by [help:night/org-present-prettify-slide-h] anyway?
  (setq org-tree-slide-modeline-display nil)
  ;; (setq org-tree-slide-modeline-display 'outside)

  (setq org-tree-slide-heading-emphasis t)
  (setq org-tree-slide-heading-level-1 '(outline-1 :height 1.2 bold))
  (setq org-tree-slide-heading-level-2 '(outline-2 :height 1.1 bold))
  (setq org-tree-slide-heading-level-3 '(outline-3 :height 1.0 bold))
  (setq org-tree-slide-heading-level-4 '(outline-4 :height 1.0 bold))

  (defun night/org-tree-slide-widen (&rest args)
    (when org-tree-slide-mode
      (widen)))

  (defun night/org-tree-slide-counsel-goto (&rest args)
    (interactive)
    (night/org-tree-slide-widen)
    (unwind-protect
        (let (
              ;; (pos (point))
              ;; (buff (current-buffer))
              (marker (point-marker))
              ;; Contains the buffer info as well? But the buffer info is probably not used anyway. We can use with-current-buffer to force the buffer. But this function can't actually jump between buffers? So who cares.
              )
          (when (call-interactively #'counsel-org-goto)
            ;; * DONE [jalali:1401/09/09/12:23] [help:org-mark-ring-push] stopped working reliably here, so I am directly using [help:night/jump-set].
            ;; ** [[id:3fe2336c-6339-4b89-a524-7cd0af6a2e8d][=doom-set-jump-a= assumes the given location is a marker]]
            ;; *** Why did things work before then?!
            ;;;
            ;; (org-mark-ring-push pos buff)
            (org-mark-ring-push marker)
            ;; (night/jump-set :pos pos)

            ;; (org-mark-ring-push)
            ))
      (when org-tree-slide-mode
        (org-tree-slide--display-tree-with-narrow))))

  (defun night/org-tree-slide-id-open (id _)
    "Go to the entry with id ID."
    (org-mark-ring-push)
    (let ((m (org-id-find id 'marker))
          cmd)
      (unless m
        (error "Cannot find entry with ID \"%s\"" id))
      ;; Use a buffer-switching command in analogy to finding files
      (setq cmd
            (or
             (cdr
              (assq
               (cdr (assq 'file org-link-frame-setup))
               '((find-file . switch-to-buffer)
                 (find-file-other-window . switch-to-buffer-other-window)
                 (find-file-other-frame . switch-to-buffer-other-frame))))
             'switch-to-buffer-other-window))
      (let
          ((goes-to-current-buffer-p
            (not (equal (current-buffer) (marker-buffer m)))))
        (if
            goes-to-current-buffer-p
            (funcall cmd (marker-buffer m))
;;;
          ;; @monkeyPatched
          (night/org-tree-slide-widen)
;;;
          )
        (goto-char m)
        (move-marker m nil)
        (org-fold-show-context)
        (night/screen-center-ni)
        )))

  (comment
   (defun night/org-present-detect-slide-h ())
   (advice-add '+org-present-detect-slide-h :override #'night/org-present-detect-slide-h)
   (advice-remove '+org-present-detect-slide-h #'night/org-present-detect-slide-h))

  (defun night/org-present-prettify-slide-h ()
    "Set up the org window for presentation."
    (let ((arg (if org-tree-slide-mode 1 -1)))
      (if (not org-tree-slide-mode)
          (when +org-present--last-wconf
            (set-window-configuration +org-present--last-wconf))
        (setq +org-present--last-wconf (current-window-configuration))
        (doom/window-maximize-buffer))
      (when (and nil
                 (fboundp 'centered-window-mode))
        (setq-local cwm-use-vertical-padding t)
        (setq-local cwm-frame-internal-border 100)
        (setq-local cwm-left-fringe-ratio -10)
        (setq-local cwm-centered-window-width 300)
        (centered-window-mode arg))
      (hide-mode-line-mode arg)
      ;; (+org-pretty-mode arg)
      (cond (org-tree-slide-mode
             (set-window-fringes nil 0 0)
             (when (bound-and-true-p flyspell-mode)
               (flyspell-mode -1))
             (add-hook 'kill-buffer-hook #'+org-present--cleanup-org-tree-slides-mode
                       nil 'local)
             (text-scale-set +org-present-text-scale)
;;;
             ;; This clears all the latex previews. I don't know why it was here, so I didn't remove it.
             (ignore-errors (org-latex-preview '(4)))

             ;; This generates all the latex previews.
             (ignore-errors (org-latex-preview '(16)))
;;;
             (night/disable-line-numbers)

             (night/org-hide-link-display)

             (advice-add 'org-id-open :override #'night/org-tree-slide-id-open)

             (advice-add
              'better-jumper-jump-backward
              :before
              #'night/org-tree-slide-widen)
             (advice-add
              'better-jumper-jump-forward
              :before
              #'night/org-tree-slide-widen))
            (t
             (text-scale-set 0)
             (pcase (type-of fringe-mode)
               ((or 'integer 'symbol) (set-window-fringes nil fringe-mode fringe-mode))
               ('cons (set-window-fringes nil (car fringe-mode) (cdr fringe-mode))))
             (org-clear-latex-preview)
             (org-remove-inline-images)
             (org-mode)
;;;
             (night/org-show-link-display)

             (advice-remove 'org-id-open #'night/org-tree-slide-id-open)

             (advice-remove
              'better-jumper-jump-backward
              #'night/org-tree-slide-widen)
             (advice-remove
              'better-jumper-jump-forward
              #'night/org-tree-slide-widen)))
      (redraw-display)))

  (advice-add '+org-present-prettify-slide-h :override #'night/org-present-prettify-slide-h)

  (defun night/org-tree-slide-move-next-tree ()
    "Display the next slide."
    (interactive)
    (when (org-tree-slide--active-p)
      (let ((msg (plist-get org-tree-slide-indicator :next))
            (message-log-max nil))
        (when msg
          (message "%s" msg)))
      (cond
       ;; displaying a slide, not the contents
       ((and (buffer-narrowed-p)
             (org-tree-slide--last-tree-p (point)))
        ;; (org-tree-slide-content)
        )
       (t
        (run-hooks 'org-tree-slide-before-move-next-hook)
        (widen)
        (org-tree-slide--outline-next-heading)
        (org-tree-slide--display-tree-with-narrow)
;;;
        (night/screen-top)
;;;
        )

       ;; stay the same slide (for CONTENT MODE, on the subtrees)
       (t (org-tree-slide--display-tree-with-narrow)))))
  (comment
   ;; overriding hotkeys instead
   (advice-add
    'org-tree-slide-move-next-tree :override
    #'night/org-tree-slide-move-next-tree))

  (defun night/org-tree-slide-move-previous-tree ()
    (interactive)
    (let
        ((org-tree-slide-skip-outline-level
          (or
           night/org-tree-slide-skip-outline-level-for-going-back
           org-tree-slide-skip-outline-level)))
      (org-tree-slide-move-previous-tree))
    (night/screen-top))
;;;
  (defvar
    night/org-tree-slide-skip-outline-level-for-going-back
    nil
    "Set to nil to use the original [help:org-tree-slide-skip-outline-level].")

  (comment
   (defun org-tree-slide--outline-previous-heading ()
     "Go to the previous heading."
     (org-tree-slide--outline-select-method
      (if (outline-previous-heading)
          (if (or
               (let
                   ((org-tree-slide-skip-outline-level
                     (or
                      night/org-tree-slide-skip-outline-level-for-going-back
                      org-tree-slide-skip-outline-level)))
                 ;; @monkeyPatched to use a custom var for determining skips when going back.

                 (org-tree-slide--heading-skip-p)))
              'skip
            nil)
        'first)
      'previous)))

  (defun night/org-tree-slide-move-previous-first-level-tree ()
    "@seeAlso [help:night/org-tree-slide-skip-outline-level-for-going-back]"
    (interactive)
    (let*
        ((org-tree-slide-skip-outline-level 2)
         ;; Show all subtrees when going back.

         (night/org-tree-slide-skip-outline-level-for-going-back
          org-tree-slide-skip-outline-level))
      (night/org-tree-slide-move-previous-tree)))

  (defun night/org-tree-slide-move-next-first-level-tree ()
    (interactive)
    (let
        ((org-tree-slide-skip-outline-level 2)
         ;; Show all subtrees when going back.
         )
      (night/org-tree-slide-move-next-tree)))
;;;
  (map! :map org-tree-slide-mode-map
        :ng "C-<" #'night/org-tree-slide-move-previous-tree
        :n [C-left] #'night/org-tree-slide-move-previous-tree
        :n [s-left] #'night/org-tree-slide-move-previous-tree
        :n [M-s-left] #'night/org-tree-slide-move-previous-first-level-tree

        :ng "C->" #'night/org-tree-slide-move-next-tree
        :n [C-right] #'night/org-tree-slide-move-next-tree
        :n [s-right] #'night/org-tree-slide-move-next-tree
        :n [M-s-right] #'night/org-tree-slide-move-next-first-level-tree)
  (map! :map org-mode-map
        :localleader
        "." #'night/org-tree-slide-counsel-goto ;; good in general
        )
  )
;;;

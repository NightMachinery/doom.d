;;; -*- lexical-binding: t; -*-
(after! outline
;;;
  ;; @toFuture/1402 Should we enable this hotkeys? We'll lose our own hotkeys defined with meta+arrow_keys. We should probably redefine the hotkeys as we have done in orgmode.
  ;;
  ;; (defvar outline-minor-mode-prefix "\M-#")
  ;; (setq outline-minor-mode-prefix "\M-#")
;;;
  (defun night/outline-goto ()
    (interactive)
    (setq ivy-calling-tmp ivy-calling)
    (let
        ((pos (point))
         (done nil))
      (unwind-protect
          (progn
            (setq-default ivy-calling t)
            (when (counsel-outline)
              (setq done t)))
        (progn
          (setq-default ivy-calling ivy-calling-tmp)
          (when (not done)
            (goto-char pos))))))

  (defun night/outline-goto-v1 (&optional regex)
    "@alt [help:night/outline-goto]"
    (interactive)
    (let (
          (re
           (or regex outline-regexp)))
      (let (
            (pos (point))
            ;; (buff (current-buffer))
            )
        (when
            (swiper-isearch
             (concat
              (night/escape-spaces-with-backslash re) " ")
             ;; [help:orderless-component-separator]
             ;; [help:orderless-escapable-split-on-space]
             )
          (night/jump-set :pos pos)
          (doom/escape)))))

  (defun night/goto-org-heading ()
    (interactive)
    (night/outline-goto-v1 "^ *\\(?:#\\|;\\|%\\|--\\|///?\\)* *[*]\\{1,8\\} ")
    ;; =///?= is to make it work with an odd number of slashes.
    )

  (defun outline-invent-heading (head up)
    ;; @PR/ready @toFuture/1402
    "Create a heading by using heading HEAD as a template.
When UP is non-nil, the created heading will be one level above.
Otherwise, it will be one level below."
    (save-match-data
      ;; Let's try to invent one by repeating or deleting the last char.
      (let* (
             (head-trailing "")
             (head
              (cond
               ((equalp (substring head -1) " ")
                (progn
                  (setq head-trailing " ")
                  (substring head 0 -1)))
               (t head)))
             (new-head (concat
                        (if up (substring head 0 -1)
                          (concat head (substring head -1)))
                        head-trailing)))
        ;; (message "head: <%s> new-head: <%s>" head new-head)
        (if (string-match (concat "\\`\\(?:" outline-regexp "\\)")
                          new-head)
            ;; Why bother checking that it is indeed higher/lower level ?
            new-head
          ;; Didn't work, so ask what to do.
          (read-string (format-message "%s heading for `%s': "
                                       (if up "Parent" "Demoted") head)
                       head nil nil t)))))

  (map! :map prog-mode-map
        :localleader
        :n "." #'night/outline-goto)
  (map!
        :localleader
        :n "/" #'night/goto-org-heading)
  (map! :map org-mode-map
        :localleader
        :n "/" #'night/goto-org-heading)
  (map! :map
        (
         ;; outshine-mode-map
         ;; outline-mode-map
         outline-minor-mode-map
         )
        :g
        "S-<left>" #'outline-previous-visible-heading
        :g
        "S-<right>" #'outline-next-visible-heading

        :g
        "M-S-<left>" #'outline-demote
        :g
        "M-S-<right>" #'outline-promote)
;;;
  )

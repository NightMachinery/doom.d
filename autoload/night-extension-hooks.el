;;; -*- lexical-binding: t; -*-

;;;
(add-hook 'find-file-hook 'night/file-extension-actions)
;; @upstreamBug https://emacs.stackexchange.com/questions/14567/find-file-hook-but-only-if-file-is-selected

(defun night/file-extension-actions ()
  (let*
      ((bfn buffer-file-name)
       (ext (or (file-name-extension bfn) "")))
    (cond
     ((and
       (not window-system)
       (member-ignore-case ext '("png" "apng" "jpg" "jpeg" "gif")))
      (night/yank-buffer-filename)
      (kill-current-buffer)
      (message "buf: %s, cmd: %s" bfn this-command)
      (night/brishz "kitty-launch-icat" bfn)
      ;; @bug this can leave the original buffer somewhat impaired, e.g., colored parens are lost in elisp mode; no idea why ...
      )
     ((member-ignore-case ext '("log" "ansitxt"))
      (scrollback-mode))
     ((member-ignore-case ext '("scrollback"))
      (with-demoted-errors
          (night/so-long-strong)
        ;; (night/so-long)

        (scrollback-mode)

        (goto-char (point-max))
        (require 'hungry-delete)
        (hungry-delete-backward-impl)
        (set-buffer-modified-p nil))
      ))))

;;;
(define-minor-mode scrollback-mode "A minor mode for browsing the termina's scrollback buffer." nil nil (make-sparse-keymap)

  (with-demoted-errors (evil-insert-state) ;; workaround to activate its map
    (evil-normal-state)
    (make-local-variable 'hlt-max-region-no-warning)
    (setq hlt-max-region-no-warning 999999999999999)
    (night/hlt-set-current-face) ;; sets the current face for =hlt-highlight-regexp-region=

    (xterm-color-colorize-buffer)
    (set-buffer-modified-p nil)
    (read-only-mode)
    ))
(map! :map scrollback-mode-map
      :nvo "q" #'save-buffers-kill-terminal

      :nvo "u" #'night/scroll-halfpage-down
      :nvo "d" #'night/scroll-halfpage-up

      :nvo "a" #'night/hlt-counsel-face
      :nvo "s" #'hlt-highlight-regexp-region
      :nvo "o" #'hlt-previous-highlight ;; @upstreamBug @todo2 these can hang if there is no highlight to be found
      :nvo "p" #'hlt-next-highlight
      )
;;;

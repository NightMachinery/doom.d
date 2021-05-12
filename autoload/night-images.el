;;; autoload/night-images.el -*- lexical-binding: t; -*-

;;;
(add-hook 'find-file-hook 'night/image-actions)
;; @upstreamBug https://emacs.stackexchange.com/questions/14567/find-file-hook-but-only-if-file-is-selected

(defun night/image-actions ()
  (let
      ((bfn buffer-file-name))
    (when (and
           (not window-system)
           (member-ignore-case (or (file-name-extension bfn) "") '("png" "apng" "jpg" "jpeg" "gif")))
        (night/yank-buffer-filename)
        (kill-current-buffer)
        (message "buf: %s, cmd: %s" bfn this-command)
        (night/brishz "kitty-launch-icat" bfn)
        ;; @bug this can leave the original buffer somewhat impaired, e.g., colored parens are lost in elisp mode; no idea why ...
        )))

;;;

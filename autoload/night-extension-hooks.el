;;; -*- lexical-binding: t; -*-

;;;
(after! (ivy counsel)
  (setq counsel-find-file-speedup-remote nil))

(add-hook 'find-file-hook 'night/file-extension-actions)
;; @upstreamBug https://emacs.stackexchange.com/questions/14567/find-file-hook-but-only-if-file-is-selected

  (defun night/org-execute-startup-block ()
    "Prepend '#+NAME: startup' to a source block, and add the appriopriate ntag to the file, and that block will run automagically when the file is opened."
    (interactive)
    (let
        ((startup-block "startup"))
      (when (member startup-block (org-babel-src-block-names))
        (save-excursion (org-babel-goto-named-src-block startup-block)
                        (org-babel-execute-src-block)))))

(defun night/file-extension-actions ()
  (let*
      ((bfn buffer-file-name)
       (ext (or (file-name-extension bfn) ""))
       (remote (s-contains? "/scp:" bfn))
       )
    ;; (message "file opened: %s" bfn)
    (when remote
      ;; (z bell-sc2-become-primal)
      ;; (z tts-glados1-cached "tramp, ready")
      (z bell-sonic-fx-ready)
      )
    (when (equalp ext "org")
      (when (s-contains? "..inline_links.." bfn)
        (make-local-variable 'buffer-invisibility-spec)
        (make-local-variable 'org-link-descriptive)
        (night/org-hide-link-display))
      (when (s-contains? "..startup.." bfn)
        ;; @securityRisk
        (night/org-execute-startup-block)))
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

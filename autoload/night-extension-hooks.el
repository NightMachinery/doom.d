;;; -*- lexical-binding: t; -*-

(defvar night/marker-audio "emacs-audio-file-playing")

(defun night/marker-audio-kill ()
  "Stop the audio file being played in the zsh session(s) marked with night/marker-audio.

Use this to stop the audio files being played by org-mode links."
  (interactive)
  (night/brishz "reval-true" "kill-marker" night/marker-audio))
;;;
(after! (ivy counsel)
  (setq counsel-find-file-speedup-remote nil))

(add-hook 'find-file-hook #'night/file-extension-actions)
(add-hook 'window-configuration-change-hook #'night/file-extension-actions2)
;; @upstreamBug https://emacs.stackexchange.com/questions/14567/find-file-hook-but-only-if-file-is-selected

  (defun night/org-execute-startup-block ()
    "Prepend '#+NAME: startup' to a source block, and add the appriopriate ntag to the file, and that block will run automagically when the file is opened."
    (interactive)
    (let
        ((startup-block "startup"))
      (when (member startup-block (org-babel-src-block-names))
        (save-excursion (org-babel-goto-named-src-block startup-block)
                        (org-babel-execute-src-block)))))

(defun night/file-extension-actions2 ()
  (with-demoted-errors
      (when buffer-file-name
        (let*
            ((bfn buffer-file-name)
             (ext (or (ignore-errors (file-name-extension bfn)) ""))
             (remote (s-contains? "/scp:" bfn)))
          ;; (message "file opened: %s" bfn)
          (cond
           ((and
             (member-ignore-case ext '("png" "apng" "jpg" "jpeg" "gif")))
            ;; (night/yank-buffer-filename)
            (z pbadd (buffer-file-name))
            (when (not window-system)
              (kill-current-buffer)
              (message "buf: %s, cmd: %s" bfn this-command)
              (night/brishz "kitty-launch-icat" bfn)
            ;; @bug this can leave the original buffer somewhat impaired, e.g., colored parens are lost in elisp mode; no idea why ...
              )
            )
           ((and
             (member-ignore-case ext '("m3u" "mp3" "ogg" "m4a" "flac" "wav" "mp4" "avi" "mkv" "flv" "wma")))
            ;; (night/yank-buffer-filename)
            (kill-current-buffer)
            (night/hear bfn)
;;;;
            ))))))

(defun night/file-extension-actions ()
  (let*
      ((bfn (or buffer-file-name ""))
       (ext (or (file-name-extension bfn) ""))
       (remote (s-contains? "/scp:" bfn)))
    ;; (message "file opened: %s" bfn)
    (when remote
      ;; (z bell-sc2-become-primal)
      ;; (z tts-glados1-cached "tramp, ready")
      (z bell-sonic-fx-ready)           ;; Remote SSH Load Bell
      )

    (cond
     ((member-ignore-case ext '("ol"))
      (night/ol-follow))

     ((member-ignore-case ext '("org"))
      (kill-local-variable 'org-link-descriptive)
      ;; [[file:~/.emacs.d.doom/.local/straight/repos/org/lisp/org.el::make-local-variable 'org-link-descriptive][newer org versions have make it a local var on all buffers, which we have undone here]]

      (when (s-contains? (ntag "inline_links") bfn)
        (make-local-variable 'buffer-invisibility-spec)

        (make-local-variable 'org-link-descriptive)

        (night/org-hide-link-display :default nil))

      (night/org-interactive-startup)

      (when (s-contains? (ntag "startup") bfn)
        ;; @securityRisk
        (night/org-execute-startup-block))

      (when (s-contains? (ntag  "org-highlighter") bfn)
        (night/org-link-highlighter-hide)
        ;; (scrollback-mode)
        (night/org-link-highlighter-hotkeys-enable)
        (org-show-all) ;; @userConfig no reason to hide stuff in a search buffer
        (org-sticky-header-mode t)
        (message "org-highlighter activated"))

      (when (night/buffer-playlistp bfn)
        ;; (night/org-playlist-hide)
        (night/org-playlist-hotkeys-enable)
        ;; (org-sticky-header-mode t)
        (message "org-playlist activated")
        )
      )

     ((member-ignore-case ext '(
                                ;; "log"
                                "ansilog"
                                "ansitxt"))
      (scrollback-mode))

     ((member-ignore-case ext '("scrollback"))
      (with-demoted-errors
          (night/so-long-strong)
        ;; (night/so-long)

        (scrollback-mode)

        (goto-char (point-max))
        (require 'hungry-delete)
        (hungry-delete-backward-impl)
        (set-buffer-modified-p nil))))))

;;;

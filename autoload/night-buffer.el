;;; ~/doom.d/night-buffer.el -*- lexical-binding: t; -*-
;;;
(after! (uniquify)
       (setq uniquify-buffer-name-style 'forward))
;;;
(defun night/force-kill-current-buffer ()
  (interactive)
  ;; [[https://emacs.stackexchange.com/questions/59348/force-kill-a-buffer][force kill a buffer? - Emacs Stack Exchange]]
  (let (kill-buffer-hook kill-buffer-query-functions)
    (kill-buffer)))

(defun night/force-kill-buffer (buffer)
  (with-current-buffer buffer
    (night/force-kill-current-buffer)))

(defun night/force-save-current-buffer ()
  (interactive)
  ;; Temporarily disable save-related hooks and functions
  (let (
        (write-file-functions nil)
        ;; `undo-fu-session--save-safe'
;;;
        (before-save-hook nil)
        (after-save-hook nil))
    (save-buffer)))
;;;
(defun night/buffer-reopen ()
  (interactive)
  (if-let (filename (or
                     buffer-file-name))
      (progn
        (kill-current-buffer)
        (let ((*night/h-interactive-buffer-force* t))
          (find-file-existing filename)))
    (error "Couldn't find filename in current buffer")))
;;;
(defun night/diff-buffers (buffer-A buffer-B)
  "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
  (interactive
   (list (read-buffer "buffer1: " (current-buffer))
         (read-buffer "buffer2: " (current-buffer))))
  ;; ediff might not be loaded, I don't know how it is loaded, but playing around with its commands does load it ...
  ;; (ediff-buffers-internal buffer-A buffer-B nil nil 'ediff-buffers)
  (ediff-buffers buffer-A buffer-B))

(defun night/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or
                     buffer-file-name
                     (and (bound-and-true-p list-buffers-directory)
                          list-buffers-directory)))
      (progn
        (kill-new filename)
        (message "Yanked: %s" filename))
    (error "Couldn't find filename in current buffer")))
(map! :leader
      "fy" #'night/yank-buffer-filename ; overrides doom's version
      )
;;;
(defun night/switch-to-last-buffer ()
  "Switch to the most recent buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(map!
 :nv
 "[[" #'night/switch-to-last-buffer)
(map! :map (evil-tex-mode-map)
 :nv
 "[[" #'night/switch-to-last-buffer)
;;;

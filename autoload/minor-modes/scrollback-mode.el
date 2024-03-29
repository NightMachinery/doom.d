;;; autoload/minor-modes/scrollback-mode.el -*- lexical-binding: t; -*-

;; (setq scrollback-mode-map (make-sparse-keymap))
(define-minor-mode scrollback-mode "A minor mode for browsing the terminal's scrollback buffer." nil nil (make-sparse-keymap)

  (with-demoted-errors (evil-insert-state) ;; workaround to activate its map
    (evil-normal-state)
    (make-local-variable 'hlt-max-region-no-warning)
    (setq hlt-max-region-no-warning 999999999999999)
    (night/hlt-set-current-face) ;; sets the current face for =hlt-highlight-regexp-region=

    (let* ((bfn (or buffer-file-name ""))
           (ext (or (file-name-extension bfn) "")))
      (cond
       ((member-ignore-case ext '("org")) t)
       (t (xterm-color-colorize-buffer)
          (set-buffer-modified-p nil)
          (read-only-mode)
          ;; =read-only-mode= activates =view-mode-map=.
          )))

;;;
    (comment
     ;; @retired keymap is shadowed by other maps anyways (probably the fault of evil maps doing unnatural hacks to get precedence)
;;;
     ;; (setq minor-mode-overriding-map-alist '(scrollback-mode-map))
     ;; (setq overriding-local-map scrollback-mode-map) ;; overrides everything so we'll lose our general keybindings
     (make-local-variable 'emulation-mode-map-alists)
     ;; doesn't seem to work
     (setq emulation-mode-map-alists (cons 'night-priority-map-alist emulation-mode-map-alists))

     ;; (setq emulation-mode-map-alists '((t scrollback-mode-map) general-maps-alist)) ;; doesn't seem to work
     )
;;;
    (map! :map 'local
          ;; https://github.com/noctuid/general.el#override-keymaps-and-buffer-local-keybindings
          ;; forcefully overrides the keybindings
;;;
          :n "q" #'save-buffers-kill-terminal
          :n [remap quit-window] #'save-buffers-kill-terminal

          :n "u" #'night/scroll-halfpage-down
          :n "d" #'night/scroll-halfpage-up

          :n "a" #'night/hlt-counsel-face
          :n "s" #'hlt-highlight-regexp-region
          :n "x" #'hlt-highlight-regexp-region

          :n "," #'hlt-previous-highlight
          :n "." #'hlt-next-highlight

          :n "o" #'link-hint-open-link
          )
    ))
;;;

(provide 'scrollback-mode)

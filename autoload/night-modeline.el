;;; autoload/night-modeline.el -*- lexical-binding: t; -*-

(after! doom-modeline
;;;
  ;; @userConfig
  (defun night/overrides-doom-modeline-buffer-file-name ()
    "Propertized variable `buffer-file-name' based on `doom-modeline-buffer-file-name-style'."
    (let* ((buffer-file-name (file-local-name (or (buffer-file-name (buffer-base-buffer)) "")))
           (buffer-file-truename (file-local-name
                                  (or buffer-file-truename (file-truename buffer-file-name) "")))
           (file-name
            (pcase doom-modeline-buffer-file-name-style
              ('auto
               (if (doom-modeline-project-p)
                   (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil nil 'hide)
                 (propertize "%b" 'face 'doom-modeline-buffer-file)))
              ('truncate-upto-project
               (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink))
              ('truncate-from-project
               (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil 'shrink))
              ('truncate-with-project
               (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink 'hide))
              ('truncate-except-project
               (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename 'shrink 'shink))
              ('truncate-upto-root
               (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename))
              ('truncate-all
               (doom-modeline--buffer-file-name-truncate buffer-file-name buffer-file-truename t))
              ('truncate-nil
               (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename))
              ('relative-to-project
               (doom-modeline--buffer-file-name-relative buffer-file-name buffer-file-truename))
              ('relative-from-project
               (doom-modeline--buffer-file-name buffer-file-name buffer-file-truename nil nil 'hide))
              ('file-name
               (propertize (file-name-nondirectory buffer-file-name)
                           'face 'doom-modeline-buffer-file))
              ((or 'buffer-name _)
               (propertize "%b" 'face 'doom-modeline-buffer-file))))
;;; @monkeyPatched
           (file-name (s-replace-regexp "^(?:cellar/)?notes/" "~nt/" file-name))
           ;; (file-name (s-replace-regexp "^doom.d/" "~dom/" file-name))
           (file-name
            (or
             (with-demoted-errors (let ((l (length file-name)))
                                    (cond
                                     ((> l 50)
                                      (concat
                                       (substring file-name 0 13)
                                       "ɣ"
                                       (substring file-name (- l 37) l)))
                                     (t file-name))))
             "error98128230124"))
;;;
           )
      (propertize (if (string-empty-p file-name)
                      (propertize "%b" 'face 'doom-modeline-buffer-file)
                    file-name)
                  'mouse-face 'mode-line-highlight
                  'help-echo (concat buffer-file-truename
                                     (unless (string= (file-name-nondirectory buffer-file-truename)
                                                      (buffer-name))
                                       (concat "\n" (buffer-name)))
                                     "\nmouse-1: Previous buffer\nmouse-3: Next buffer")
                  'local-map mode-line-buffer-identification-keymap)))

  (advice-add 'doom-modeline-buffer-file-name :override #'night/overrides-doom-modeline-buffer-file-name)
  ;; This is ultimately where the modeline is constructed:
  ;; [[doom:.local/straight/repos/doom-modeline/doom-modeline-segments.el::(doom-modeline-def-segment buffer-info]]

  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
;;;
  (doom-modeline-def-segment system-name
    (propertize (system-name) 'face 'mode-line))

  ;; [[doom:.local/straight/repos/doom-modeline/doom-modeline.el::(doom-modeline-def-modeline 'main]]
  (let* ((LHS
          '(
            eldoc
            ;; bar
            workspace-name
            window-number
            modals
            matches
            follow
            buffer-info
            remote-host
            buffer-position
            word-count
            parrot
            selection-info
            ))
         (RHS
          '(
            input-method
            compilation
            objed-state
            misc-info
            persp-name
            battery
            grip
            irc
            mu4e
            gnus
            github
            debug
            repl
            lsp
            minor-modes
            indent-info
            buffer-encoding
            major-mode
            process
            vcs
            check
            time
            ))
         (LHS (cond ((night/remote-p) (append (subseq LHS 0 4) '(system-name) (subseq LHS 4)))
                    (t LHS))))
    (doom-modeline-def-modeline 'main LHS RHS))

  ;; [[doom:.local/straight/repos/doom-modeline/doom-modeline.el::(doom-modeline-def-modeline 'dashboard]]
  (doom-modeline-def-modeline 'dashboard
    '(window-number modals system-name buffer-default-directory-simple remote-host)
    '(compilation misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process time))
  )

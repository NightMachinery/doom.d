(after! (doom-modeline-core)
  (setq doom-modeline-icon nil)         ;; makes the modeline way too big

  (cond ((display-graphic-p)
         (setq doom-modeline-height 1)  ;; doesn't seem to work
         ;; (doom-adjust-font-size 2) ;; adjusting the GUI emacs font size
;;;

         (setq evil-insert-state-cursor '(box "purple"))

         ;; (setq evil-normal-state-cursor '(box "royalblue"))
         ;; (setq evil-insert-state-cursor '((bar . 5) "yellow"))
         )
        (t
         ;; [[nightNotes:subjects/software/tools, apps, utility/CLI/terminal emulators/escape codes/change cursor type.org][escape codes/change cursor type.org]]
         ;;
         ;; These do not support colors.
         ;;
         ;; (global-term-cursor-mode)
         ;;
         ;; https://github.com/7696122/evil-terminal-cursor-changer
         ;;
         ;; (setq evil-insert-state-cursor '(box "azure"))
         ;; (setq evil-normal-state-cursor '(box "paleturquoise"))

         ;; [[NIGHTDIR:configFiles/kitty/themes/night-solarized-light.conf][themes/night-solarized-light.conf]]
         ;; (setq evil-insert-state-cursor '(box "azure"))
         (setq evil-insert-state-cursor '(box "#f19618"
                                              ;; orange
                                              ))
         (setq evil-normal-state-cursor '(box "#c7ceff"))

         (require 'evil-terminal-cursor-changer)
         (evil-terminal-cursor-changer-activate)
         )))
;;;
(after! highlight
  (setq hlt-max-region-no-warning 999999999999999))
;;;
;; How to make the highlights re-use the original background color? https://github.com/tarsius/hl-todo/issues/60 ; did not work:
;; :background "unspecified"
;; :inherit t
(defface special-comment '((t (:inherit t :foreground "#3437eb" :weight bold))) "This is just a doc string")

;;;

;; white-green background: #e9f2eb
(defface zsh-macro '((t (:foreground "#1adb51" :background "#e9f1f2" :weight bold))) "This is just a doc string")
(defface urgent-face '((t (
                           :foreground "black"
                           ;; :background "hotpink"
                           :background "mistyrose"
                           ;; :background "orangered"
                           :weight bold))) "")

(defun night/highlight-atsign ()
  "DEPRECATED: We now customize hl-todo instead."
  (interactive)
  (progn (font-lock-add-keywords
          ;; [:punct:]
          nil '(("\\B\\(@[^][[:space:](){};\n\"=]+\\)" 1 'special-comment t)))))

(defun night/highlight-atsign-zsh ()
  (interactive)
  (progn (font-lock-add-keywords 'sh-mode
                                 '(("\\B\\(@[^][[:space:](){};\n\"=]+\\)" 1 'zsh-macro t)))))

(setq night/great-tag-regex "^.*\\(@great\\>\\|:great:\\|@forked\\|:forked:\\).*$")
(setq night/urgent-tag-regex "^.*\\(@urgent\\|:urgent:\\|@ASAP\\|:ASAP:\\).*$")
(defun night/highlight-org ()
  (interactive)
  (progn
    ;; (font-lock-add-keywords 'org-mode
    ;;                         '(("^.*\\B@great\\B.*$" 1 'zsh-macro t)))
    (font-lock-add-keywords 'org-mode
                            `((,night/great-tag-regex . 'zsh-macro))
                            )
    (font-lock-add-keywords 'org-mode
                            `((,night/urgent-tag-regex
                               .
                               ;; 'dired-broken-symlink
                               'urgent-face)))
    )
  )

(night/highlight-atsign-zsh)
(night/highlight-org)
;;; tests
;; jas (@wsw aws) @hi+ @hi? maddah_ali@sharif.edu hi@gmail.com TODO @ XXXX @ja
;;a@a
;;@hi j
;;;
(after! hl-todo
  ;; The syntax class of the characters at either end has to be `w' (which means word) in `hl-todo--syntax-table'.
  (modify-syntax-entry ?@ "w" hl-todo--syntax-table)
  (modify-syntax-entry ?+ "w" hl-todo--syntax-table)
  (modify-syntax-entry ?? "w" hl-todo--syntax-table)
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(
          ;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold)
          ("@[^][[:space:](){};\n\"=]+" special-comment)
          ))
  )
;;;
(defun night/hl-line-range-function ()
  (cons (line-end-position) (line-beginning-position 2)))
(defun night/hl-line-start-from-end ()
  (setq hl-line-range-function #'night/hl-line-range-function)
  ;; (setq hl-line-sticky-flag t)
  ;; (setq global-hl-line-sticky-flag t)
  )

;;;
(defun night/highlight-background ()
  "Highlights the background of the hardcoded patterns on the current buffer. It does NOT react to new additions of the patterns in the buffer. The highlight also is kind of sticky. It uses overlays so at least it's not overrided by hl-line, but then again, it erases the at-tags as they are not yet overlays ..."
  (interactive)
  (hlt-highlight-regexp-region nil nil night/great-tag-regex 'zsh-macro 1))
(defun night/hlt-set-current-face (&optional face)

  (let ((face
         (or face 'zsh-macro)))
    (setq hlt-last-face face)
    ;; (hlt-highlight-regexp-region 0 0 "" face 1)
    )

  (comment
   (night/hlt-set-current-face 'special-comment)))

(defun night/hlt-counsel-face ()
  (interactive)
  (night/hlt-set-current-face
   ;; (counsel-faces)
   (let* ((names (mapcar #'symbol-name (face-list)))
          (counsel--faces-format
           (format "%%-%ds %%s"
                   (apply #'max 0 (mapcar #'string-width names)))))
     (ivy-read "Face: " names
               :require-match t
               :history 'face-name-history
               ;; :preselect hlt-last-face
               :preselect "hlt-regexp-level-5"
               :caller 'counsel-faces))))
;;;
(defun night/zero-width-chars-hide (&optional global)
  (interactive)
  (when (not global)
    (setq-local glyphless-char-display (copy-sequence glyphless-char-display)))
  (set-char-table-range glyphless-char-display
                        (char-from-name "ZERO WIDTH SPACE") 'zero-width))

(defun night/zero-width-chars-show (&optional global)
  (interactive)
  (when (not global)
    (setq-local glyphless-char-display (copy-sequence glyphless-char-display)))
  (set-char-table-range glyphless-char-display
                        (char-from-name "ZERO WIDTH SPACE") 'thin-space))

(add-hook 'org-mode-hook #'night/zero-width-chars-hide)
;;;
(defun night/theme-set-local-buffer (&optional theme)
  "buggy, not recommended, but it still works"
  (interactive)
  (let ((theme (or theme
                   'humanoid-light
                   'doom-one)))
    (require 'load-theme-buffer-local)
    (load-theme-buffer-local theme (current-buffer))
    (night/h-local-theme-workaround)))

(defun night/h-local-theme-workaround ()
  ;; @bug also disables our highlighting of at-tags, and I can't fix it even invoking `night/highlight-atsign'
  ;; @bug it generally messes up some details of the main theme
  (enable-theme night-theme) ;; @workaround for reseting the main theme to some random theme
  )

(after! (counsel)
  (defun counsel-load-theme-action (x)
  "Disable current themes and load theme X."
  (condition-case nil
      (progn
        (mapc #'disable-theme custom-enabled-themes)
        (let ((theme (intern x)))
          (setq night-theme theme)      ;; @monkeyPatched
          (load-theme theme t))
        (when (fboundp 'powerline-reset)
          (powerline-reset)))
    (error "Problem loading theme %s" x))))
;;;

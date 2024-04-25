;;;
(setq night/at-tag-char-regex "[^][[:space:](){};\n\"=]")
(setq night/at-tag-main-char-regex "[a-zA-Z0-9?!]")
(setq night/at-tag-regex
      (concat "\\B\\(@" night/at-tag-char-regex "+\\)"))
(setq night/at-tag-main-regex
      (concat "\\B\\(@" night/at-tag-main-char-regex "+\\)"))

(setq night/great-tag-regex
      ;; "^.*\\(@great\\>\\|:great:\\|@forked\\|:forked:\\|@idea/accepted\\)[^[:space:]]*"
      (concat "\\(@great\\>\\|:great:\\|@forked\\|:forked:\\|@idea/accepted\\)" night/at-tag-char-regex "*")
      ;; "^.*\\(@great\\>\\|:great:\\|@forked\\|:forked:\\|@idea/accepted\\).*$"
      )
(setq night/urgent-tag-regex "^.*\\(@urgent\\>\\|:urgent:\\|@ASAP\\|:ASAP:\\).*$")
(setq night/CR-tag-regex (concat "\\(@CR\\>\\)" night/at-tag-char-regex "*"))
(setq night/done-tag-regex (concat "\\(@done\\>\\)" night/at-tag-char-regex "*"))
(setq night/moved-tag-regex (concat "\\(@moved\\>\\)" night/at-tag-char-regex "*"))


;; white-green background: #e9f2eb
(night/defface zsh-macro '((t
                            (
                             :foreground "#1adb51"
                             ;; :background "#e9f1f2"
                             :weight bold)))
  "Face used for global aliases in Zsh")
(night/defface urgent-face '((t (
                           :foreground "black"
                                       ;; :background "hotpink"
                           :background "mistyrose"
                           ;; :background "orangered"
                           :weight bold))) "")
(night/defface CR-face '((t (
                           :foreground "MintCream"
                           ;; :foreground "thistle1"
                           ;; :foreground "black"

                           :background "orange1"
                           :weight bold))) "")
(night/defface done-face '((t (
                               ;; :foreground "MintCream"
                               ;; :foreground "PaleGreen"
                               :foreground "DarkGreen"

                               :background "LimeGreen"
                               ;; :background "PaleGreen"
                               ;; :background "LawnGreen"
                               ;; :background "GreenYellow"
                               ;; :background "Green"
                               :weight bold))) "")
;;;
(defun night/disable-line-numbers ()
  (interactive)
  (display-line-numbers-mode -1))

(defun night/enable-line-numbers ()
  (interactive)
  (display-line-numbers-mode))
;;;
(cl-defun night/read-char-or-cancel
    (&optional prompt inherit-input-method seconds
               &key (cancel-chars '(27)))
  "Read a character from the user, but cancel if one of CANCEL-CHARS is pressed.
CANCEL-CHARS is a list of characters that will trigger a cancellation.
By default, it contains only the escape character (27)."
  (let ((char (read-char prompt inherit-input-method seconds)))
    (if (memq char cancel-chars)
        (progn
          (message "Cancelled!")
          nil)
      char)))
;;;
(require 'ov)
(cl-defun night/ov (&key
                    beg end
                    properties
                    front-advance
                    rear-advance
                    )
  "Make an overlay from BEG to END.

If PROPERTIES are specified, set them for the created overlay."
  (if properties
      (progn
        ;; To pass properties to `ov-set'
        (when (listp (car-safe properties))
          (setq properties (car properties)))
        (let ((o (ov-make beg end nil front-advance rear-advance)))
          (ov-set o properties)
          o))
    (ov-make beg end nil front-advance rear-advance)))
;;;
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
(cond
 ((facep 'doom-themes-org-at-tag)
  (set-face-attribute 'doom-themes-org-at-tag nil :weight 'bold)
  ;; (copy-face 'doom-themes-org-at-tag 'at-tag-face)
  ;; (set-face-attribute 'at-tag-face nil :weight 'bold)
;;;
  (put 'at-tag-face 'face-alias 'doom-themes-org-at-tag)
  )
 (t
  (night/defface at-tag-face
  '((t (:inherit t :foreground "#3437eb" :weight bold)))
  "Face of at-tags")))

;;;
(night/defface night/async-insertion-face
  '((t (
        :foreground "black"
        :background "orchid1"
        ;; :weight bold
        :extend t
        ;; extend makes the face go to the end of the line, but it also makes it extend to new lines if you create them. This latter property seems like an upstream bug.
        ))) "")

(defun night/highlight-atsign ()
  "DEPRECATED: We now customize hl-todo instead."
  (interactive)
  (progn (font-lock-add-keywords
          ;; [:punct:]
          nil `((,night/at-tag-regex 1 'at-tag-face t)))
         (font-lock-fontify-buffer)))

(defun night/highlight-atsign-zsh ()
  (interactive)
  (progn
    (font-lock-add-keywords
     'sh-mode
     `((,night/at-tag-main-regex 1 'zsh-macro t)))
    (font-lock-fontify-buffer)))

(defun night/highlight-org ()
  (interactive)
  (comment
   ;; @broken @bug
   ;; When these tags are matched, the styling of the rest of the line gets affected. E.g., org headings lose their styling.
   ;; So I have disabled them for now.
   ;; [help:night/highlight-background] works, though it needs manual invocation to update the colors.
   (font-lock-add-keywords
    'org-mode
    ;; @duplicateCode/e637f800f6aefb7d629b8e5dad00aa36
    `(
      (,night/urgent-tag-regex
       .
       ;; 'dired-broken-symlink
       'urgent-face)
      (,night/CR-tag-regex
       .
       'CR-face)
      (,night/done-tag-regex
       .
       'done-face)
      (,night/great-tag-regex . 'zsh-macro)
      ))
   (font-lock-fontify-buffer)
   )
  )

(night/highlight-atsign-zsh)
(night/highlight-org)
;;; tests
;; jas (@wsw aws) @hi+ @hi? maddah_ali@sharif.edu hi@gmail.com TODO @ XXXX @ja
;;a@a
;;@hi j
;;;
(require 'hl-todo)
(after! hl-todo
  ;; The syntax class of the characters at either end has to be `w' (which means word) in `hl-todo--syntax-table'.
  (modify-syntax-entry ?@ "w" hl-todo--syntax-table)
  (modify-syntax-entry ?+ "w" hl-todo--syntax-table)
  (modify-syntax-entry ?? "w" hl-todo--syntax-table)
  (setq hl-todo-highlight-punctuation ":")
  (customize-set-variable 'hl-todo-keyword-faces
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
          ("MONKEY" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold)
          (
           ;; night/at-tag-regex
           ;; ,(concat "@" night/at-tag-char-regex "+")
           ,(concat "@" night/at-tag-main-char-regex "+")
           at-tag-face)
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
(defvar-local night/highlight-overlays nil
  "Buffer-local variable to store the highlight overlays.")

(defun night/highlight-background ()
  "Highlights the background of the hardcoded patterns on the current buffer.
It removes the previous highlights before applying new ones."
  (interactive)
  (let ((debug-p nil))
    ;; Remove previous highlight overlays
    (when night/highlight-overlays
      (when debug-p
        (message "Removing %d previous highlight overlays" (length night/highlight-overlays)))
      (dolist (overlay night/highlight-overlays)
        (delete-overlay overlay))
      (setq night/highlight-overlays nil))

    ;; Apply new highlights and store the overlays
    (dolist
        (item
         ;; @duplicateCode/e637f800f6aefb7d629b8e5dad00aa36
         `((,night/great-tag-regex 'zsh-macro)
           (,night/CR-tag-regex 'CR-face)
           (,night/done-tag-regex 'done-face)
           (,night/urgent-tag-regex 'urgent-face)
           (,night/moved-tag-regex 'table-cell)))
      (let ((regex (car item))
            (face (cadr item)))
        (let ((hlt-overlays nil))
          ;; `hlt-overlays' is a dynamic global var that is set in `hlt-highlight-region'.
          (hlt-highlight-regexp-region (point-min) (point-max) regex face 'MSGP)
          (when debug-p
            (message "Created overlays: %s" hlt-overlays))
          (setq night/highlight-overlays (append night/highlight-overlays hlt-overlays)))))))

(defun night/hlt-set-current-face (&optional face)

  (let ((face
         (or face 'zsh-macro)))
    (setq hlt-last-face face)
    ;; (hlt-highlight-regexp-region 0 0 "" face 1)
    )

  (comment
   (night/hlt-set-current-face 'at-tag-face)))

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
(defun night/theme-org-override ()
  "Override some of the org-mode faces.

Alternatively, `(setq modus-themes-org-blocks 'gray-background)`.
"
  (interactive)
  (let* ((frame nil)
         (inside-bg "#eeeeee")
         (begin-bg inside-bg)
         ;; (begin-bg "#dddddd")
         (end-bg begin-bg)
         (extend t) ;; extend the background to the end of line
         (begin-line nil)
         (end-line nil)
         ;; @? =:overline= doesn't work on TUI.
         ;; (begin-line t)
         ;; (end-line t)
         )
    ;; Set the foreground of the `org-hide' face to the background color of the frame:
    (let ((bg-color (cdr (assoc 'background-color (frame-parameters)))))
      (set-face-attribute 'org-hide nil :foreground bg-color))

    (cond
     ((not (memq 'kaolin-light custom-enabled-themes))
      ;; Override org-block
      (set-face-attribute 'org-block frame
                          :background inside-bg
                          ;; :foreground "#000000"
                          :extend extend)

      ;; Override org-block-begin-line
      (set-face-attribute 'org-block-begin-line frame
                          :background begin-bg
                          :overline begin-line
                          :extend extend)

      ;; Override org-block-end-line
      (set-face-attribute 'org-block-end-line frame
                          :background end-bg
                          :underline end-line
                          :extend extend)))))
(after! (org org-faces)
  (add-hook 'org-mode-hook 'night/theme-org-override))
;;;
(defvar night/active-overlays nil
  "List of active overlays. (Used by: `night/flash-region').")

(defun night/clear-overlays ()
  "Clear all overlays in `night/active-overlays`."
  (mapc 'delete-overlay night/active-overlays)
  (setq night/active-overlays nil))

(add-hook! 'doom-escape-hook 'night/clear-overlays)

(cl-defun night/flash-region
    (start end &key (delay 0.5) (backend 'overlay-timer) (face 'done-face))
  "Temporarily highlight region from START to END.
DELAY specifies the duration of the highlight in seconds; defaults to 0.5.
If DELAY is t, the highlight is persistent until `C-g` is pressed.
BACKEND specifies the backend to use for flashing; defaults to 'overlay-timer.
FACE specifies the face to use for flashing; defaults to 'done-face."
  (cl-case backend
    (overlay-timer
     (let ((overlay (make-overlay start end)))
       (overlay-put overlay 'face face)
       (push overlay night/active-overlays)
       (unless (eq delay t)
         (run-with-timer delay nil (lambda (ov)
                                     (delete-overlay ov)
                                     (setq night/active-overlays (remove ov night/active-overlays)))
                         overlay))))
    (nav-flash
     (if (eq delay t)
         (error "nav-flash backend does not support indefinite delay")
       (nav-flash-show start end face delay)))
    (t
     (error "Invalid backend: %s" backend))))
;;;
(provide 'night-ui)

;;; night-org-inline-copy.el --- DWIM-copy inline (=...=, ~...~) and block contents -*- lexical-binding: t; -*-

;; Author: You
;; Version: 0.7
;; Package-Requires: ((emacs "27.1") (org "9.1"))
;; Keywords: outlines, convenience
;; URL: https://example.invalid/night-org-inline-copy

;;; Commentary:
;;
;; When point is on an Org inline object like =verbatim= or ~code~, copy the
;; object's value and skip the original DWIM action. Additionally, when point
;; is inside common block elements, copy just the block contents.
;;
;; Designed to advise Doom's `+org/dwim-at-point` if present; otherwise, you
;; can call `night/org-inline-copy-dwim` interactively or wire it into your
;; own DWIM command.
;;
;; Key refactor notes (v0.7):
;; - Centralized “compute payload + flash range” for inline & blocks
;; - Single copy+flash function with optional trimming
;; - Kept fixed-width semantics (payload via :value, flash actual region)
;; - Same user options and surface API

;;; Code:

(after! (org night-org-overrides evil)
  (require 'org-element)
  (require 'subr-x)
  (require 'evil-traces)

  (defgroup night/org-inline-copy nil
    "Copy inline Org contents when triggering DWIM."
    :group 'org
    :prefix "night/org-inline-copy-")

  (defcustom night/org-inline-copy-inline '(verbatim code)
    "Inline Org objects that trigger copying."
    :type '(repeat (choice (const verbatim)
                           (const code)))
    :group 'night/org-inline-copy)

  (defcustom night/org-inline-copy-blocks
    '(example-block src-block export-block special-block dynamic-block comment-block
      quote-block verse-block center-block
      fixed-width)
    "Block-level Org elements that trigger copying."
    :type '(repeat (choice (const example-block)
                           (const src-block)
                           (const export-block)
                           (const special-block)
                           (const dynamic-block)
                           (const comment-block)
                           (const quote-block)
                           (const verse-block)
                           (const center-block)
                           (const fixed-width)))
    :group 'night/org-inline-copy)

  (defcustom night/org-inline-copy-trim-blocks t
    "When non-nil, trim trailing newline(s) when copying block text."
    :type 'boolean
    :group 'night/org-inline-copy)

  ;; ---------- Visuals ----------

  (defun night/org-inline-copy--flash (beg end)
    "Briefly highlight region from BEG to END."
    (night/flash-region beg end :delay 0.1 :face 'evil-traces-copy-preview))

  (defun night/org-inline-copy--kill (string &optional trimp)
    "Put STRING on the kill-ring; when TRIMP is non-nil, trim trailing newlines."
    (kill-new (if trimp (string-trim-right string) string)))

  ;; ---------- Bounds helpers ----------

  (defun night/org-inline-copy--fence-ranges (b e)
    "Best-effort (CB . CE) inside Org BEGIN/END fences between B and E.
Understands both \"#+BEGIN_…/#+END_…\" and \"#+BEGIN:/#+END:\" styles.
Returns nil if no suitable fences are detected."
    (save-excursion
      (let (cb ce)
        (goto-char b)
        (let ((case-fold-search t))
          (when (looking-at-p "^\\s-*#\\+BEGIN\\(?:[_:].*\\)?\\s-*$")
            (forward-line 1))
          (setq cb (point))
          (goto-char e)
          (when (re-search-backward "^\\s-*#\\+END\\(?:[_:].*\\)?\\s-*$" b t)
            (setq ce (match-beginning 0))))
        (when (and cb ce (< cb ce))
          (cons cb ce)))))

  (defun night/org-inline-copy--contents-bounds (ctx)
    "Return a cons (CB . CE) for the *contents* of block CTX, or nil.
Preference order:
1) Use :contents-begin/:contents-end when both exist.
2) Use fence heuristics for #+BEGIN…/#+END… or #+BEGIN:/#+END: blocks.
3) For fixed-width, use element bounds minus post-blank.
Otherwise return nil."
    (let* ((cb (org-element-property :contents-begin ctx))
           (ce (org-element-property :contents-end   ctx))
           (b  (org-element-property :begin ctx))
           (e  (org-element-property :end   ctx))
           (pb (or (org-element-property :post-blank ctx) 0))
           (typ (org-element-type ctx)))
      (cond
       ;; 1) Org-provided content bounds
       ((and (integerp cb) (integerp ce) (< cb ce))
        (cons cb ce))
       ;; 2) Fences
       ((night/org-inline-copy--fence-ranges b e))
       ;; 3) Fixed-width: raw element minus post-blank
       ((eq typ 'fixed-width)
        (let ((cb2 b) (ce2 (- e pb)))
          (and (< cb2 ce2) (cons cb2 ce2))))
       (t nil))))

  ;; ---------- Central “compute” path ----------

  (defun night/org-inline-copy--compute (ctx)
    "Compute payload and flash range for CTX.
Returns a plist: (:payload STRING :flash (BEG . END)) or nil.
For fixed-width, payload is from :value (strips leading colons) while the
flash range matches buffer contents (so visuals reflect the real region)."
    (let* ((typ (org-element-type ctx))
           (b   (org-element-property :begin ctx))
           (e   (org-element-property :end   ctx))
           (pb  (or (org-element-property :post-blank ctx) 0)))
      (cond
       ;; Inline objects
       ((memq typ night/org-inline-copy-inline)
        (let* ((val (org-element-property :value ctx))
               ;; markers are one char on each side (= or ~)
               (ib  (1+ b))
               (ie  (- e pb 1)))
          (when (and val (integerp ib) (integerp ie) (< ib ie))
            (list :payload val :flash (cons ib ie)))))

       ;; Blocks
       ((memq typ night/org-inline-copy-blocks)
        (let* ((bounds  (night/org-inline-copy--contents-bounds ctx))
               (cb      (car bounds))
               (ce      (cdr bounds))
               ;; payload choice:
               ;; - fixed-width => :value (drop leading colons)
               ;; - others => prefer exact buffer slice so flash matches precisely
               (payload (if (eq typ 'fixed-width)
                            (org-element-property :value ctx)
                          (or (and cb ce (buffer-substring-no-properties cb ce))
                              (org-element-property :value ctx)))))
          (when (and (stringp payload))
            (list :payload payload :flash (and cb ce (cons cb ce))))))

       (t nil))))

  (defun night/org-inline-copy--copy-dispatch (ctx)
    "Copy contents computed from CTX and flash the range when available.
Always returns non-nil when CTX belongs to configured inline/block sets,
so that callers can choose to suppress the original DWIM."
    (let ((info (night/org-inline-copy--compute ctx)))
      (cond
       ((not info)
        ;; We matched a supported element but couldn't compute a payload.
        ;; Keep behavior gentle: just say nothing to copy.
        (message "Nothing to copy.")
        t)
       (t
        (let* ((payload (plist-get info :payload))
               (range   (plist-get info :flash)))
          (if (and payload (> (length payload) 0))
              (progn
                (night/org-inline-copy--kill
                 payload
                 ;; Only trim for blocks (inline markers rarely include trailing NLs)
                 (memq (org-element-type ctx) night/org-inline-copy-blocks)
                 )
                (when (consp range)
                  (night/org-inline-copy--flash (car range) (cdr range))))
            (message "Block is empty; nothing to copy."))
          t)))))

  ;; ---------- Entry points ----------

  (defun night/org-inline-copy--maybe-copy-at-point ()
    "Copy inline or block content at point if it matches user prefs.
Return non-nil if we handled the situation (even if nothing was copied)."
    (let* ((ctx0 (org-element-context))
           ;; If inside a paragraph within a block, climb to the block.
           (ctx  (or (org-element-lineage ctx0 night/org-inline-copy-blocks t) ctx0))
           (typ  (and ctx (org-element-type ctx))))
      (cond
       ((memq typ night/org-inline-copy-inline)
        (night/org-inline-copy--copy-dispatch ctx))
       ((memq typ night/org-inline-copy-blocks)
        (night/org-inline-copy--copy-dispatch ctx))
       (t nil))))

  (defun night/org-inline-copy-dwim (&optional _arg)
    "If point is on an inline object or supported block, copy its contents.
Otherwise, do nothing (return nil). Intended for use as a DWIM action."
    (interactive "P")
    (when (derived-mode-p 'org-mode)
      (night/org-inline-copy--maybe-copy-at-point)))

  ;; ---------- Doom integration ----------

  (defun night/org-inline-copy--around-dwim (orig-fn &rest args)
    "Around advice to inject copying behavior into ORIG-FN with ARGS."
    (if (derived-mode-p 'org-mode)
        (condition-case err
            (if (night/org-inline-copy--maybe-copy-at-point)
                ;; We handled it; skip original DWIM.
                nil
              ;; Otherwise, fall back to original DWIM.
              (apply orig-fn args))
          (error
           (message "night/org-inline-copy error: %s"
                    (error-message-string err))
           (apply orig-fn args)))
      (apply orig-fn args)))

  ;; Only advise if Doom's DWIM exists.
  ;; (when (fboundp '+org/dwim-at-point))
  (advice-add '+org/dwim-at-point :around #'night/org-inline-copy--around-dwim)

  (provide 'night-org-inline-copy)
  )
;;; night-org-inline-copy.el ends here

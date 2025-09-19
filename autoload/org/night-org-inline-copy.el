;;; night-org-inline-copy.el --- DWIM-copy inline (=...=, ~...~) and block contents -*- lexical-binding: t; -*-

;; Author: You
;; Version: 0.6
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
;; Highlights:
;; - Inline objects: verbatim (=...=) and code (~...~)
;; - Blocks (configurable defaults): example/src/export/special/dynamic/comment,
;;   quote/verse/center, and fixed-width
;; - First tries :contents-begin / :contents-end when available; otherwise
;;   falls back to robust fence detection (#+BEGIN…/#+END… or #+BEGIN:/#+END:)
;; - Fixed-width: copies payload **without** leading colons (via :value),
;;   but flashes the actual buffer region
;; - Gentle visual flash of the copied region

;;; Code:

(after! org
  (require 'org-element)
  (require 'subr-x)

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

  (defun night/org-inline-copy--flash (beg end)
    "Briefly highlight region from BEG to END."
    (cond
     ((fboundp 'night/flash-region)
      (ignore-errors
        (night/flash-region beg end :delay 0.1 :face 'evil-traces-copy-preview)))
     ((fboundp 'pulse-momentary-highlight-region)
      (ignore-errors
        (pulse-momentary-highlight-region beg end)))
     (t
      (let ((ov (make-overlay beg end)))
        (overlay-put ov 'face 'highlight)
        (run-with-timer 0.15 nil #'delete-overlay ov)))))

  (defun night/org-inline-copy--copy-inline (ctx)
    "Copy inline object value from CTX (verbatim/code)."
    (let ((val (org-element-property :value ctx)))
      (if (not val)
          (message "No value found for copying!")
        (kill-new val)
        (let* ((b (org-element-property :begin ctx))
               (e (org-element-property :end ctx))
               (pb (or (org-element-property :post-blank ctx) 0))
               ;; Markers for =...= or ~...~ are one char on each side:
               (ib (1+ b))
               (ie (- e pb 1)))
          (when (< ib ie)
            (night/org-inline-copy--flash ib ie))))))

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
       ;; 1) Explicit contents bounds provided by Org
       ((and (integerp cb) (integerp ce) (< cb ce))
        (cons cb ce))

       ;; 2) Try BEGIN/END fence heuristic (covers example/src/export/special/dynamic/comment)
       ((let ((pair (night/org-inline-copy--fence-ranges b e)))
          (and pair pair)))

       ;; 3) Fixed-width: no fences; use raw element bounds minus post-blank
       ((eq typ 'fixed-width)
        (let ((cb2 b) (ce2 (- e pb)))
          (and (< cb2 ce2) (cons cb2 ce2))))

       ;; No luck
       (t nil))))

  (defun night/org-inline-copy--copy-block (ctx)
    "Copy block contents from CTX.
Uses :contents-begin/:contents-end when available; otherwise,
falls back to fence-based ranges or (for fixed-width) the raw element."
    (let* ((typ    (org-element-type ctx))
           (bounds (night/org-inline-copy--contents-bounds ctx))
           (cb     (car bounds))
           (ce     (cdr bounds))
           ;; Choose payload for the kill ring:
           ;; - For fixed-width, always use :value to drop leading colons.
           ;; - Otherwise, prefer the buffer substring so flashing matches exactly.
           (payload (if (eq typ 'fixed-width)
                        (org-element-property :value ctx)
                      (or (and cb ce (buffer-substring-no-properties cb ce))
                          (org-element-property :value ctx)))))
      (if (and payload (> (length payload) 0))
          (progn
            (kill-new (if night/org-inline-copy-trim-blocks
                          (string-trim-right payload)
                        payload))
            (when (and cb ce) (night/org-inline-copy--flash cb ce)))
        (message "Block is empty; nothing to copy."))))

  (defun night/org-inline-copy--maybe-copy-at-point ()
    "Copy inline or block content at point if it matches user prefs.
Return non-nil if something was copied."
    (let* ((ctx0 (org-element-context))
           ;; If we're inside a paragraph within a block, climb to the block:
           (ctx (or (org-element-lineage ctx0 night/org-inline-copy-blocks t) ctx0))
           (typ (and ctx (org-element-type ctx))))
      (cond
       ((memq typ night/org-inline-copy-inline)
        (night/org-inline-copy--copy-inline ctx)
        t)
       ((memq typ night/org-inline-copy-blocks)
        (night/org-inline-copy--copy-block ctx)
        t)
       (t nil))))

  (defun night/org-inline-copy-dwim (&optional _arg)
    "If point is on an inline object or supported block, copy its contents.
Otherwise, do nothing (return nil). Intended for use as a DWIM action."
    (interactive "P")
    (when (derived-mode-p 'org-mode)
      (night/org-inline-copy--maybe-copy-at-point)))

  ;; Advice Doom's +org/dwim-at-point if available, otherwise do nothing.
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
  (when (fboundp '+org/dwim-at-point)
    (advice-add '+org/dwim-at-point :around #'night/org-inline-copy--around-dwim))

  (provide 'night-org-inline-copy)
  )
;;; night-org-inline-copy.el ends here

;;; night-evil-org-block-textobj.el --- Evil text objects for Org blocks on `#` -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Evil text objects bound to `#` for selecting Org “blocks”.
;; - inner:  i#  → block contents (EXCLUDES the BEGIN/END lines and the final newline)
;; - outer:  a#  → whole block (INCLUDES BEGIN/END; EXCLUDES post-blank by default)
;;
;; Supported elements: src-block, example-block, verse-block, quote-block,
;; center-block, special-block, export-block, comment-block, dynamic-block.
;;
;; Notes:
;; - OUTER ends at the BOL of the line *after* the END line, so no trailing blank
;;   lines are selected. If you prefer including post-blank, set
;;   `night/evil-org-block-textobj-outer-include-post-blank` to non-nil.
;; - INNER ends at EOL of the line before END (exclusive), so no final newline.

;;; Code:

(after! org
  (defgroup night/evil-org-block-textobj nil
    "Evil text objects for Org block elements."
    :group 'org
    :prefix "night/evil-org-block-textobj-")

  (defcustom night/evil-org-block-textobj-types
    '(src-block example-block verse-block quote-block center-block
      special-block export-block comment-block dynamic-block)
    "Org element types treated as blocks for this text object."
    :type '(repeat (choice (const src-block)
                           (const example-block)
                           (const verse-block)
                           (const quote-block)
                           (const center-block)
                           (const special-block)
                           (const export-block)
                           (const comment-block)
                           (const dynamic-block)))
    :group 'night/evil-org-block-textobj)

  (defcustom night/evil-org-block-textobj-outer-include-post-blank nil
    "If non-nil, `a#` includes post-blank lines after the END line.
When nil (default), `a#` excludes post-blank and selects exactly
from the BEGIN line through the END line."
    :type 'boolean
    :group 'night/evil-org-block-textobj)

  (defcustom night/evil-org-block-textobj-debug nil
    "When non-nil, emit debug messages for text object computations."
    :type 'boolean
    :group 'night/evil-org-block-textobj)

  (defun night/evil-org-block-textobj--dbg (fmt &rest args)
    "Debug printer controlled by `night/evil-org-block-textobj-debug`."
    (when night/evil-org-block-textobj-debug
      (apply #'message (concat "[evil-org-block#] " fmt) args)))

  (defun night/evil-org-block-textobj--line-col (pos)
    "Return (LINE . COL) for POS."
    (save-excursion
      (goto-char pos)
      (cons (line-number-at-pos) (current-column))))

  (defun night/evil-org-block-textobj--at-point ()
    "Return the nearest enclosing supported block element, or nil."
    (let ((elem (org-element-context)))
      (while (and elem
                  (not (memq (org-element-type elem)
                             night/evil-org-block-textobj-types)))
        (setq elem (org-element-property :parent elem)))
      elem))

  (defun night/evil-org-block-textobj--bounds (outerp)
    "Return (BEG . END) bounds for the Org block at point.
If OUTERP is non-nil, return whole block bounds (BEGIN..END line). Otherwise
return inner bounds:
  - Start (CB): first char on the line after BEGIN.
  - End   (CE): end-of-line of the line *before* END (exclusive)."
    (let ((blk (night/evil-org-block-textobj--at-point)))
      (when blk
        (let* ((type (org-element-type blk))
               (beg  (org-element-property :begin blk))
               (end  (org-element-property :end   blk))
               (post (or (org-element-property :post-blank blk) 0)))
          (save-excursion
            ;; BEGIN line BOL
            (goto-char beg)
            (let ((beg-bol (line-beginning-position)))
              ;; BOL(:end) is after the element and *all* post-blank.
              (goto-char end)
              (let* ((after-element-bol (line-beginning-position))
                     ;; BOL of END line: step back (post + 1) lines
                     (_ (forward-line (- (1+ post))))
                     (end-line-bol (line-beginning-position))
                     ;; BOL of the line right AFTER the END line
                     (after-end-bol (save-excursion
                                      (goto-char end-line-bol)
                                      (forward-line 1)
                                      (line-beginning-position)))
                     ;; INNER start: first char after BEGIN line
                     (cb (save-excursion
                           (goto-char beg-bol)
                           (forward-line 1)
                           (point)))
                     ;; INNER end (exclusive): EOL of the line *before* END
                     (ce (save-excursion
                           (goto-char end-line-bol)
                           (forward-line -1)
                           (end-of-line)
                           (point))))
                (if outerp
                    ;; OUTER:
                    ;; If including post-blank: use BOL(:end).
                    ;; Else (default): stop at the line right after END, excluding post-blank.
                    (let* ((outer-beg beg-bol)
                           (outer-end (if night/evil-org-block-textobj-outer-include-post-blank
                                          after-element-bol
                                        after-end-bol)))
                      (night/evil-org-block-textobj--dbg
                       "OUTER type=%s :begin=%d :end=%d post=%d  OUTER=(%d,%d) L=%S..%S  include-post-blank=%s"
                       type beg end post outer-beg outer-end
                       (night/evil-org-block-textobj--line-col outer-beg)
                       (night/evil-org-block-textobj--line-col (max outer-beg (1- outer-end)))
                       night/evil-org-block-textobj-outer-include-post-blank)
                      (cons outer-beg outer-end))
                  ;; INNER: clamp empty blocks to zero-width at CB.
                  (when (< ce cb) (setq ce cb))
                  (night/evil-org-block-textobj--dbg
                   "INNER type=%s :begin=%d :end=%d post=%d  CB=%d CE(excl)=%d  CB@%S CE-1@%S"
                   type beg end post cb ce
                   (night/evil-org-block-textobj--line-col cb)
                   (and (> ce cb)
                        (night/evil-org-block-textobj--line-col (1- ce))))
                  (cons cb ce)))))))))

  (after! evil
    (evil-define-text-object night/evil-a-org-block (count &optional _beg _end _type)
      "Select an Org block (BEGIN..END). Post-blank excluded by default."
      (let ((bounds (and (derived-mode-p 'org-mode)
                         (night/evil-org-block-textobj--bounds t))))
        (unless bounds
          (user-error "Not inside an Org block"))
        ;; Line-wise feels natural for the outer object.
        (evil-range (car bounds) (cdr bounds) 'line :expanded t)))

    (evil-define-text-object night/evil-inner-org-block (count &optional _beg _end _type)
      "Select inside an Org block (without BEGIN/END lines; no final newline)."
      (let ((bounds (and (derived-mode-p 'org-mode)
                         (night/evil-org-block-textobj--bounds nil))))
        (unless bounds
          (user-error "Not inside an Org block"))
        ;; Exclusive: ends at EOL of the last content line → no final newline.
        (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t)))

    ;; Bind to `#`:  i# (inner), a# (outer)
    (define-key evil-inner-text-objects-map (kbd "#") #'night/evil-inner-org-block)
    (define-key evil-outer-text-objects-map (kbd "#") #'night/evil-a-org-block)))

(provide 'night-evil-org-block-textobj)
;;; night-evil-org-block-textobj.el ends here

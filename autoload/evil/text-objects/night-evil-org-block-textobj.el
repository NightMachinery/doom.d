;;; night-evil-org-block-textobj.el --- Evil text objects for Org blocks on `#` -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Provides Evil text objects bound to `#` for selecting Org “blocks”.
;; - inner:  i#  → the block’s contents (between BEGIN/END lines)
;; - outer:  a#  → the whole block (including BEGIN/END and trailing blank)
;;
;; Supported block-like elements:
;; src-block, example-block, verse-block, quote-block, center-block,
;; special-block, export-block, comment-block, dynamic-block.
;;
;; Doom note: uses `after!` instead of `require`/`with-eval-after-load`.

;;; Code:

;; Everything lives under `after! org` so Org is present when we use org-element.
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

  (defun night/evil-org-block-textobj--at-point ()
    "Return the org-element for the enclosing block (per custom types), or nil."
    (let* ((ctx0 (org-element-context))
           (blk  (org-element-lineage ctx0 night/evil-org-block-textobj-types t)))
      blk))

  (defun night/evil-org-block-textobj--bounds (outerp)
    "Return (BEG . END) bounds for block at point.
If OUTERP is non-nil, return full block bounds (:begin..:end), extended to whole
lines for nicer selection. Otherwise return contents bounds
(:contents-begin..:contents-end), trimming a trailing newline."
    (let ((blk (night/evil-org-block-textobj--at-point)))
      (when blk
        (if outerp
            (let ((b (org-element-property :begin blk))
                  (e (org-element-property :end blk)))
              (save-excursion
                (goto-char b)
                (setq b (line-beginning-position))
                (goto-char e)
                ;; :end is at bol after the block
                (setq e (line-beginning-position))
                (cons b e)))
          (let ((cb (org-element-property :contents-begin blk))
                (ce (org-element-property :contents-end blk)))
            (when (and cb ce (> ce cb))
              (save-excursion
                (let ((b cb) (e ce))
                  (goto-char e)
                  (when (and (> e b) (eq (char-before e) ?\n))
                    (setq e (1- e)))
                  (cons b e)))))))))

  ;; Define and bind the text objects only after Evil is available.
  (after! evil
    (evil-define-text-object night/evil-a-org-block (count &optional _beg _end _type)
      "Select an Org block (including BEGIN/END)."
      (let ((bounds (and (derived-mode-p 'org-mode)
                         (night/evil-org-block-textobj--bounds t))))
        (unless bounds
          (user-error "Not inside an Org block"))
        ;; Line-wise selection feels natural for outer objects.
        (evil-range (car bounds) (cdr bounds) 'line :expanded t)))

    (evil-define-text-object night/evil-inner-org-block (count &optional _beg _end _type)
      "Select the inside of an Org block (without BEGIN/END)."
      (let ((bounds (and (derived-mode-p 'org-mode)
                         (night/evil-org-block-textobj--bounds nil))))
        (unless bounds
          (user-error "Not inside an Org block"))
        (evil-range (car bounds) (cdr bounds) 'inclusive :expanded t)))

    ;; Bind to `#`:  i# (inner), a# (outer)
    (define-key evil-inner-text-objects-map (kbd "#") #'night/evil-inner-org-block)
    (define-key evil-outer-text-objects-map (kbd "#") #'night/evil-a-org-block)))

(provide 'night-evil-org-block-textobj)
;;; night-evil-org-block-textobj.el ends here

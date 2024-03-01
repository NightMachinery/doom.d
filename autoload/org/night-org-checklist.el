;;; autoload/org/night-org-checklist.el -*- lexical-binding: t; -*-

(after! (org evil-org org-list)
  (defun night/org-modify-checklist-item (action)
    "Modify the current checklist item based on ACTION.
ACTION can be 'cancel, 'uncancel, or 'toggle'."

    (let*
        ((item-symbol-pattern "\\(?:-\\|\\+\\)")
         (pattern
          (concat
           ;; [[id:0e68bf6b-173c-4a4c-8f28-bc096695eb64][=\\s-=]]
           "^\\(\\s-*"
           "\\)\\("
           item-symbol-pattern
           "\\)\\s-+\\(\\[[^]]+\\]\\)\\s-+\\(.*\\)$")))
      (kill-new pattern)
      (save-excursion
        (beginning-of-line)
        ;; Check if we're at a checklist item, considering optional plus signs
        ;; and handling both empty and filled checkboxes
        (when (looking-at pattern)
          ;; Extract the leading whitespace, checkbox, and the item text
          (let* ((indentation (match-string 1))
                 (item-symbol (match-string 2))
                 (checkbox (match-string 3))
                 (item-text (match-string 4))
                 (item-text (s-trim item-text))
                 (is-cancelled
                  (and (string-prefix-p "+" item-text)
                       (string-suffix-p "+" item-text))))

            ;; Determine the new item text based on the action
            (setq item-text
                  (cond
                   ((eq action 'cancel)
                    (unless is-cancelled (concat "+" item-text "+")))
                   ((eq action 'uncancel)
                    (when is-cancelled (substring item-text 1 -1)))
                   ((eq action 'toggle)
                    (if is-cancelled (substring item-text 1 -1) (concat "+" item-text "+")))))
            ;; Replace the current line with the modified item, if there's a change
            (when item-text
              (delete-region (line-beginning-position) (line-end-position))
              (insert (format "%s%s %s %s" indentation item-symbol checkbox item-text))))))))

  (defun night/org-checklist-cancel-current-item ()
    "Cancel the current checklist item by surrounding it with plus signs."
    (interactive)
    (night/org-modify-checklist-item 'cancel))

  (defun night/org-checklist-uncancel-current-item ()
    "Uncancel the current checklist item by removing surrounding plus signs."
    (interactive)
    (night/org-modify-checklist-item 'uncancel))

  (defun night/org-checklist-toggle-current-item ()
    "Toggle the cancellation state of the current checklist item."
    (interactive)
    (night/org-modify-checklist-item 'toggle))
;;;
  (defun night/org-checklist-toggle-partial ()
    (interactive)
    ;; This currently doesn't toggle, it always marks as =[-]=.
    ;; We should implement the toggling ourselves.
    (org-toggle-checkbox '(16)))
;;;
  (map! :map 'evil-org-mode-map
        :localleader
        "xx" #'org-toggle-checkbox
        "xc" #'night/org-checklist-toggle-current-item
        "xz" #'night/org-checklist-toggle-partial
        ))

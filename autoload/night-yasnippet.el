;;; autoload/night-yasnippet.el -*- lexical-binding: t; -*-
;;;
(require 'yasnippet)

(add-to-list 'warning-suppress-types '(yasnippet backquote-change))
;;;
(cl-defun night/h-point-at-beginning-p
    (&key
     prefix-pattern
     (fixed-strings nil))
  "Check if the point is at the beginning of a line followed by PREFIX-PATTERN."
  (let ((point-start (point))
        (line-start (line-beginning-position))

        (pattern (if fixed-strings
                     (regexp-quote prefix-pattern)

                   prefix-pattern)))
    (save-excursion
      (goto-char line-start)
      (when (looking-at pattern)
        (let ((match-end (match-end 0)))
          (= point-start match-end))))))

(defun night/h-yas-active-field ()
  "Return Yasnippet's active field when it is structurally valid."
  (let ((active-field (and (overlayp yas--active-field-overlay)
                           (overlay-get yas--active-field-overlay 'yas--field))))
    (when (and active-field
               (yas--field-p active-field))
      active-field)))

(defun night/h-yas-field-live-snippet (field)
  "Return the active snippet covering FIELD, or nil for stale FIELD state."
  (car (yas-active-snippets (yas--field-start field)
                            (yas--field-end field))))

(defun night/h-yas-clear-stale-active-field ()
  "Clear stale active-field overlays left after Yasnippet state drifts."
  (when (overlayp yas--active-field-overlay)
    (delete-overlay yas--active-field-overlay))
  (when (boundp 'yas--field-protection-overlays)
    (mapc #'delete-overlay yas--field-protection-overlays)
    (setq yas--field-protection-overlays nil)))

(defun night/h-yas-next-field-around (oldfun &rest args)
  "Avoid `yas-next-field' errors when Yasnippet has stale field state."
  (let ((active-field (night/h-yas-active-field)))
    (cond
     ((and active-field
           (night/h-yas-field-live-snippet active-field))
      (apply oldfun args))
     (t
      (night/h-yas-clear-stale-active-field)
      nil))))

(unless (advice-member-p #'night/h-yas-next-field-around #'yas-next-field)
  (advice-add #'yas-next-field :around #'night/h-yas-next-field-around))
;;;
(after! yasnippet
  (setq +snippets-dir (concat (getenv "DOOMDIR") "/" "night-snippets/"))
  (setq yas-snippet-dirs (list +snippets-dir))
  ;; having too many snippets is bad. We can use the function =+snippets/find-for-current-mode= to see what is available from where.
  ;; Elements appearing earlier in the list override later elements'
  ;; snippets.
  ;; The first directory is taken as the default for storing snippet's
  ;; created with yas-new-snippet.
  ;; Snippets named `__` are file templates.
  ;; These two were the defaults that we just reset:
  (add-to-list 'yas-snippet-dirs '+file-templates-dir 'append #'eq)
  (add-to-list 'yas-snippet-dirs 'doom-snippets-dir 'append #'eq)

  (defun night/h-yas-rm-snippets (&optional &rest dummy)
    (interactive)
    (yas--remove-template-by-uuid (yas--table-get-create 'org-mode) "quote"))
  (advice-add #'yas-reload-all :after #'night/h-yas-rm-snippets )

  (yas-reload-all t)
;;;
  (setq yas-key-syntaxes ;; @userConfig
        (list #'yas-try-key-from-whitespace
              "w_.()" "w_." "w_"))
;;;
  (after! night-last
    (yas-reload-all t)
    ;;;
    ;; @seeAlso [help:night/company-keybindings-enable]
    (map!
     :map yas-minor-mode-map
     :ig
     "TAB" yas-maybe-expand
     :ig
     "<tab>" yas-maybe-expand
     :ig
     "\t" yas-maybe-expand
     :ig
     [?\t] yas-maybe-expand)

    (map!
     :map yas-keymap
     :ig
     "TAB" (yas-filtered-definition 'yas-next-field-or-maybe-expand)
     :ig
     "<tab>" (yas-filtered-definition 'yas-next-field-or-maybe-expand)
     :ig
     "\t" (yas-filtered-definition 'yas-next-field-or-maybe-expand)
     :ig
     [?\t] (yas-filtered-definition 'yas-next-field-or-maybe-expand))

    (comment
     (map!
      :map (yas-minor-mode-map)
      :ig
      "TAB" #'yas-expand
      :ig
      "<tab>" #'yas-expand
      :ig
      "\t" #'yas-expand
      :ig
      [?\t] #'yas-expand)
     (map!
      :map (yas-minor-mode-map)
      :ig
      "TAB" #'night/company-yasnippet-or-completion
      :ig
      "<tab>" #'night/company-yasnippet-or-completion
      :ig
      "\t" #'night/company-yasnippet-or-completion
      :ig
      [?\t] #'night/company-yasnippet-or-completion)))
;;;
  )

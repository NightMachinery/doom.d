;;; autoload/org/night-org-keybindings.el -*- lexical-binding: t; -*-

(after! (org evil-org evil)
;;;
(defun night/yank-org-inner-object ()
  (interactive)
  (apply #'kill-ring-save (evil-org-select-inner-element (org-element-context))))

(defun evil-org-select-inner-element (element)
  "Select inner org ELEMENT."
  (let ((type (org-element-type element))
        (begin (org-element-property :begin element))
        (end (org-element-property :end element))
        (contents-begin (org-element-property :contents-begin element))
        (contents-end (org-element-property :contents-end element))
        (post-affiliated (org-element-property :post-affiliated element))
        (post-blank (org-element-property :post-blank element)))
    (cond
;;; @monkeyPatched
     ((memq type '(link))
      (let*
          ((link-type (org-element-property :type element))
           (link-type-len (length link-type))
           (link-path (org-element-property :path element))
           (inner-begin
            (save-excursion
              (goto-char begin)
              (re-search-forward "\\w\\|~" end)
              (point)))
           (path-begin
            (cond
             ((or
               (not (equalp
                     (buffer-substring-no-properties (- inner-begin 1) (+ inner-begin link-type-len -1))
                     link-type))
               (comment (member link-type '("fuzzy"))))
              (- inner-begin 1))
             ((member link-type '("http" "https" "ftp"))
              (setq link-path (concat link-type ":" link-path))
              (- inner-begin 1))
             (t (+ inner-begin link-type-len))))
           (link-path-len (length link-path))
           (path-end (+ path-begin link-path-len)))

        (list path-begin path-end)))
;;;
     ((or (string-suffix-p "-block" (symbol-name type))
          (memq type '(latex-environment)))
      ;; Special case on block types (thanks Nicolas Goaziou)
      (list (org-with-point-at post-affiliated (line-beginning-position 2))
            (org-with-point-at end (line-beginning-position (- post-blank)))))
     ((memq type '(verbatim code))
      (list (1+ begin) (- end post-blank 1)))
     ('otherwise
      (list (or contents-begin post-affiliated begin)
            (or contents-end
                ;; Prune post-blank lines from :end element
                (org-with-point-at end
                  ;; post-blank is charwise for objects and linewise for elements
                  (if (memq type org-element-all-objects)
                      (- end post-blank)
                    (line-end-position (- post-blank))))))))))
;;;
  (defun night/insert-zero-width-space ()
    (interactive)
    (insert "\u200b"))
;;;
  (defvar night/org-support-shift-select 'heading)

  (defun night/org-shiftright ()
    (interactive)
    (cond
     ((or
       (eq night/org-support-shift-select 'heading)
       (and (not (eq org-support-shift-select 'always))
            (org-at-heading-p)))

      (org-next-visible-heading 1)
      t)

     (t nil)))

  (defun night/org-shiftleft ()
    (interactive)
    (cond
     ((or
       (eq night/org-support-shift-select 'heading)
       (and (not (eq org-support-shift-select 'always))
            (org-at-heading-p)))

      (org-next-visible-heading -1)
      t)

     (t nil)))

  (defun night/org-shiftright-final ()
    (interactive)
    (org-next-visible-heading 1))

  (defun night/org-shiftleft-final ()
    (interactive)
    (org-next-visible-heading -1))

  (add-hook 'org-shiftleft-hook #'night/org-shiftleft)
  (add-hook 'org-shiftright-hook #'night/org-shiftright)
  (add-hook 'org-shiftleft-final-hook #'night/org-shiftleft-final)
  (add-hook 'org-shiftright-final-hook #'night/org-shiftright-final)
;;;
  (map! :map org-mode-map
        :localleader
        :nvi "lp" #'night/org-paste-clipboard-image
        "la" #'night/semantic-scholar-to-org
        "rp" #'night/org-paste-with-files)
  (setq org-startup-folded 'overview) ; @upstreambug https://github.com/hlissner/doom-emacs/issues/3693
  (map!
   :map (org-mode-map evil-org-mode-map)
   :n
   ;; "TAB" 'org-cycle
   ;; "<S-tab>" 'org-force-cycle-archived ; is overrided ...
   "TAB" 'org-force-cycle-archived
   ;; (setq org-cycle-open-archived-trees t)  ;; https://emacs.stackexchange.com/questions/64067/expand-an-archived-subtree-with-just-tab/

   :nvo
   "g8" #'night/avy-goto-org-header

   :n
   "gl" #'org-open-at-point

   :n
   "zx" #'org-redisplay-inline-images

   :nvo
   "{" #'org-previous-visible-heading

   :nvo
   "}" #'org-next-visible-heading

   :nivo "C-<up>" #'org-backward-heading-same-level
   :nivo "C-<down>" #'org-forward-heading-same-level

   :nvo "C-S-<left>" #'org-backward-heading-same-level
   :nvo "C-S-<right>" #'org-forward-heading-same-level

   :nvoi "C-M-<up>" #'org-babel-previous-src-block
   :nvoi "C-M-<down>" #'org-babel-next-src-block

   :i
   "M-S-<left>" #'org-promote-subtree   ; already bound in normal mode
   :localleader
   "rs" #'avy-org-refile-as-child
   ;; "sr" #'avy-org-refile-as-child
   "rf" #'+org/refile-to-file
   "rF" #'night/org-refile-to-new-file



   "lC" #'night/url2org
   "lc" #'night/org-link-browser-current
   "le" #'night/org-link-edge-current
   ))

;;; autoload/org/night-org-keybindings.el -*- lexical-binding: t; -*-

(after! (org evil-org evil night-backlinks)
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

      (night/org-next-visible-heading 1)
      t)

     (t nil)))

  (defun night/org-shiftleft ()
    (interactive)
    (cond
     ((or
       (eq night/org-support-shift-select 'heading)
       (and (not (eq org-support-shift-select 'always))
            (org-at-heading-p)))

      (night/org-previous-visible-heading 1)
      t)

     (t nil)))

  (defun night/org-shiftright-final ()
    (interactive)
    (night/org-next-visible-heading 1))

  (defun night/org-shiftleft-final ()
    (interactive)
    (night/org-previous-visible-heading 1))

  (add-hook 'org-shiftleft-hook #'night/org-shiftleft)
  (add-hook 'org-shiftright-hook #'night/org-shiftright)
  (add-hook 'org-shiftleft-final-hook #'night/org-shiftleft-final)
  (add-hook 'org-shiftright-final-hook #'night/org-shiftright-final)
;;;
  ;; `night/org-next-visible-heading' used to be an :override advice on
  ;; `org-next-visible-heading' with the fold-skipping disabled, which made it
  ;; land on headings hidden inside folds. The advice is gone; this keeps
  ;; reloading this file safe when a stale copy is still installed in a running
  ;; Emacs (otherwise the command below would recurse into itself).
  ;; See [[file:~/doom.d/docs/org-heading-navigation.md]].
  (advice-remove 'org-next-visible-heading 'night/org-next-visible-heading)

  (defun night/org--goto-visible-heading (n)
    "Move to the Nth visible heading, or not at all.
N is negative to move backward.  Unlike `org-next-visible-heading',
point does not move when there is no visible heading left in that
direction, instead of jumping to the end (or the beginning) of the
buffer.  Return non-nil when point moved."
    (let ((origin (point)))
      (org-next-visible-heading n)
      (cond
       ((and (org-at-heading-p) (not (org-invisible-p (point) t))) t)
       (t
        (goto-char origin)
        ;; `night/screen-center-ni' has already fired via its :after advice on
        ;; `org-next-visible-heading', so re-center on the restored point.
        (night/screen-center-ni)
        nil))))

  (defun night/org-next-visible-heading (arg)
    "Move to the next visible heading line.
With ARG, repeats or can move backward if negative.

Unlike `org-next-visible-heading', point stays put when there is no
further visible heading."
    (interactive "p")
    (night/org--goto-visible-heading arg))

  (defun night/org-previous-visible-heading (arg)
    "Move to the previous visible heading line.
With ARG, repeats or can move forward if negative.

Unlike `org-previous-visible-heading', point stays put when there is no
further visible heading."
    (interactive "p")
    (night/org--goto-visible-heading (- arg)))

;;;
  (defun night/org-move-less-nested-heading (direction)
  "Move to the next or previous heading that is less nested than the current one.
DIRECTION should be 'next or 'previous.

Only visible headings are considered, and point stays put when there is
no such heading, instead of running to the end (or the beginning) of the
buffer."
  (let ((step (if (eq direction 'next) 1 -1))
        (current-level (or
                        (org-current-level)
                        0))
        (found nil))
    ;; (unless current-level
    ;;   (error "Not on an org heading"))
    (save-excursion
      (while (and (not found)
                  (night/org--goto-visible-heading step))
        (let ((level (org-current-level)))
          (when (and level
                     (or (<= level 1)
                         (< level current-level)))
            (setq found (point))))))
    (when found
      (goto-char found))
    found))

(defun night/org-next-less-nested-heading ()
  "Move to the next heading that is less nested than the current one."
  (interactive)
  (night/org-move-less-nested-heading 'next))

(defun night/org-previous-less-nested-heading ()
  "Move to the previous heading that is less nested than the current one."
  (interactive)
  (night/org-move-less-nested-heading 'previous))
;;;
  (map! :map org-mode-map
        :localleader
        :n "] ]" #'org-beamer-export-to-pdf
        :nvi "lp" #'night/org-paste-clipboard-image
        :nvi "lP" #'night/org-img-unused-trs
        "la" #'night/semantic-scholar-to-org
        "lA" #'night/arxiv-to-org
        "lm" #'night/org-link-hear-get
        "lM" #'night/org-link-mpv-get
        "rp" #'night/org-paste-with-files
        ;; "yi" #'night/org-value-at-point
        )
  (setq org-startup-folded
        'nofold
        ;; 'overview
        ) ; @upstreambug https://github.com/hlissner/doom-emacs/issues/3693

  (map!
   :map (org-mode-map evil-org-mode-map)
   :n
   ;; "TAB" 'org-cycle
   ;; "<S-tab>" 'org-force-cycle-archived ; is overrided ...
   "TAB" 'org-force-cycle-archived
   ;; (setq org-cycle-open-archived-trees t)  ;; https://emacs.stackexchange.com/questions/64067/expand-an-archived-subtree-with-just-tab/

   ;; :nvig
   ;; "s-v" #'night/org-paste-escaped-smart

   :nvo
   "g8" #'night/avy-goto-org-header

   :n
   "gl"
   #'org-open-at-point-global
   ;; #'org-open-at-point
   ;; :n "gL" #'org-insert-link-global

   :n
   "zx"
   #'night/org-redisplay-images-etc
   ;; #'org-redisplay-inline-images

   :n
   "zo"
   #'org-fold-show-subtree
   ;; #'+org/open-fold

   :n "zO" #'org-fold-show-all

   :nvo "{" #'night/org-previous-visible-heading
   :nvo "}" #'night/org-next-visible-heading

   ;; :nivo "C-<up>" #'org-backward-heading-same-level
   ;; :nivo "C-<down>" #'org-forward-heading-same-level
   ;; :nivo "C-M-<up>" #'org-backward-heading-same-level
   ;; :nivo "C-M-<down>" #'org-forward-heading-same-level


   ;; :nvoi "C-M-<up>" #'org-babel-previous-src-block
   ;; :nvoi "C-M-<down>" #'org-babel-next-src-block
   ;; :nvoi "C-<up>" #'org-babel-previous-src-block
   ;; :nvoi "C-<down>" #'org-babel-next-src-block
   :nvoi "C-<up>" #'night/org-babel-previous-src-block
   :nvoim "C-<down>" #'night/org-babel-next-src-block
   :nvoi "C-," #'night/org-babel-previous-src-block
   :nvoi "C-." #'night/org-babel-next-src-block

   :nvo "C-M-<left>" #'night/org-previous-less-nested-heading
   :nvo "C-M-<right>" #'night/org-next-less-nested-heading
   :nvo "C-M-h" #'night/org-previous-less-nested-heading
   :nvo "C-M-l" #'night/org-next-less-nested-heading

   :nvo "C-<left>" #'org-backward-heading-same-level
   :nvo "C-<right>" #'org-forward-heading-same-level
   ;; :nvo "C-h" #'org-backward-heading-same-level
   ;; :nvo "C-l" #'org-forward-heading-same-level

   :nvo "H"
   #'org-shiftleft

   :nvo "L"
   #'org-shiftright

   :nvo "C-h"
   #'org-backward-heading-same-level
   ;; #'org-shiftleft
   :nvo "C-l"
   #'org-forward-heading-same-level
   ;; #'org-shiftright

   :nvoig
   "C-k"
   #'evil-scroll-up
   :nvoig
   "C-j"
   #'evil-scroll-down

   :i
   "M-S-<left>" #'org-promote-subtree   ; already bound in normal mode
   :localleader
   "rs" #'avy-org-refile-as-child
   ;; "sr" #'avy-org-refile-as-child
   "rf" #'+org/refile-to-file
   "rF" #'night/org-refile-to-new-file

   "ui" #'orgmdb-fill-movie-properties
   ;; Update IMDB
   ;; Perhaps we should add an after-advice to run =+org/open-all-folds= or =revert-buffer=? There is a bug that the inserted properties drawer can't be opened without running a workaround first ...revert-buffer a bug that the inserted properties drawer can't be opened without running a workaround first ...

   "lk" #'night/org-backlink-search
   "lc" #'night/org-link-chrome-current
   "lC" #'night/url2org
   "le" #'night/org-link-edge-current
   "ls" #'night/org-link-safari-current
   "l;" #'night/org-link-browser-current
   "l:" #'night/url2org)
  (map!
   :leader
   "lc" #'night/org-link-chrome-current
   "lC" #'night/url2org
   "le" #'night/org-link-edge-current
   "ls" #'night/org-link-safari-current
   "l;" #'night/org-link-browser-current
   "l:" #'night/url2org))

;;; B
;;; ~
;;; /doom.d/autoload/night-ivy.el -*- lexical-binding: t; -*-

(with-eval-after-load 'ivy
  ;;;
  (setq ivy-height 14)
;;;
  (defun night/ivy--no-sort (name candidates)
    candidates)

  (add-to-list 'ivy-sort-matches-functions-alist '(t . ivy--shorter-matches-first))
  (add-to-list 'ivy-sort-matches-functions-alist '(ivy-switch-buffer . ivy-sort-function-buffer))
  (add-to-list 'ivy-sort-matches-functions-alist '(counsel-bookmark . night/ivy--no-sort))
  (add-to-list 'ivy-sort-matches-functions-alist '(+ivy/jump-list . night/ivy--no-sort))
  (add-to-list 'ivy-sort-matches-functions-alist '(counsel-imenu . night/ivy--no-sort))
  (add-to-list 'ivy-sort-matches-functions-alist '(counsel-org-goto . night/ivy--no-sort))
  (add-to-list 'ivy-sort-matches-functions-alist '(counsel-outline . night/ivy--no-sort))
  (add-to-list 'ivy-sort-matches-functions-alist '(counsel-recentf . night/ivy--no-sort))

  (comment
   (add-to-list 'ivy-sort-matches-functions-alist '(counsel-fzf . ivy--shorter-matches-first))
   (add-to-list 'ivy-sort-matches-functions-alist '(counsel-fzf . night/ivy--no-sort))
   (setq ivy-sort-matches-functions-alist
         (delete '(counsel-fzf . night/ivy--no-sort) ivy-sort-matches-functions-alist)))


  ;; (setq ivy-sort-matches-functions-alist '((t . ivy--shorter-matches-first)))

  ;; (setq ivy-sort-matches-functions-alist '((t . ivy--flx-sort)))
  ;; @upstreamBug `ivy--flx-sort' does not work with orderless. It's also slow.
;;;
  (if t
      ;; activate for all:
      (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))

    ;; just replace the default option:
    (setq ivy-re-builders-alist (a-assoc-1 ivy-re-builders-alist t 'orderless-ivy-re-builder)))

  ;; `ivy--regex-ignore-order's '!negation' is broken for me
  (add-to-list 'ivy-re-builders-alist '(swiper-all . ivy--regex-ignore-order)) ;; needed by `night/swiper-irc-me'
  (add-to-list 'ivy-re-builders-alist '(counsel-org-goto . ivy--regex-ignore-order))
  ;; (add-to-list 'ivy-re-builders-alist '(counsel-rg . ivy--regex-ignore-order))

  ;;;
  (defun night/ivy--regex-pcre (str)
    "@buggy e.g., capture groups hang."
    (let ((re-elisp (pcre-to-elisp str)))
      ;; (message "re-elisp: %s" re-elisp)

      ;; re-elisp
      ;; (orderless-ivy-re-builder re-elisp) ;; does NOT work
      ;; (ivy--regex-ignore-order re-elisp) ;; does NOT work
      (ivy--regex-plus re-elisp)
      ))
  ;; NOTE Alias `swiper--re-builder' instead of changing `ivy-re-builders-alist'.
  ;; (defalias #'swiper--re-builder #'night/ivy--regex-pcre)
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . night/ivy--regex-pcre))
  ;; (add-to-list 'ivy-re-builders-alist '(swiper . ivy--regex-ignore-order))
  ;;;
  ;; @alt to orderless:
  ;; - `ivy--regex-ignore-order'
  ;; - Ivy has ivy-restrict-to-matches, bound to S-SPC, so you can get the effect of out of order matching without using ivy--regex-ignore-order: (@toFuture/1401/6 this might be faster?)
  ;;  (define-key ivy-minibuffer-map (kbd "SPC") #'ivy-restrict-to-matches)
  ;;  (define-key ivy-minibuffer-map (kbd "SPC") #'self-insert-command)

  (progn
    (defvar *orderless-no-fuzzy* nil)
    (defun night/h-orderless-style-dispatcher (pattern index _total)
      ;; makes `counsel-recentf' too slow
      ;;
      ;; you can bind `orderless-style-dispatchers' dynamically to override this for specific commands
      (cond
       ((string= "!" pattern) `(orderless-literal . ""))
       ;; Without literal
       ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
       ;; Character folding
       ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
       ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
       ;; Initialism matching
       ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
       ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
       ;; Literal matching
       ((string-prefix-p "'" pattern) `(orderless-literal . ,(substring pattern 1)))
       ((string-suffix-p "'" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
       ;; Flex matching
       ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
       ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))
       ((and (not *orderless-no-fuzzy*)
             (= _total 1)) 'orderless-flex)
       (t 'orderless-regexp)))
    (defun night/h-orderless-regexp (pattern index _total)
      'orderless-regexp)

    (setq orderless-style-dispatchers '(night/h-orderless-style-dispatcher))

    ;; @great this can support an essentially arbitrary query syntax:
    ;;   https://github.com/oantolin/orderless#style-dispatchers
    (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism))
    (comment
     ;; fuzzy (orderless-flex) breaks `night/search-notes'
     (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism orderless-flex))))

  (defun night/advice-orderless-no-fuzzy (orig-fn &rest args)
    (let ((*orderless-no-fuzzy* t))
      (apply orig-fn args)))
  (advice-add 'counsel-rg :around #'night/advice-orderless-no-fuzzy)
  (advice-add 'swiper-isearch :around #'night/advice-orderless-no-fuzzy)
;;;
  (comment
   (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))))

(after! (ivy counsel ivy-rich)
  (setq counsel-find-file-ignore-regexp nil) ;; @tradeoff @config

;;;
  (setq ivy-truncate-lines t)
  ;; @workaround turn this off using a local let bind on relevant functions

  ;; (setq ivy-truncate-lines nil)
  ;; otherwise, we can't see long paths
  ;; @upstreamBug ivy-rich adds extra stuff that it assumes will be just truncated, so it doesn't play nicely with this:
  ;;   https://github.com/Yevgnen/ivy-rich/issues/112

  (defun night/counsel-recentf-advice (orig-fn)
    (let ((ivy-truncate-lines nil)
          ;; (orderless-style-dispatchers #'night/h-orderless-regexp)
          (*orderless-no-fuzzy* t)
          )
      (funcall orig-fn)))
  (advice-add #'counsel-recentf :around #'night/counsel-recentf-advice)
;;;

  (defun night/ivy--directory-out ()
    (interactive)
    (let (dir)
      (when (and
             ;; instead of ivy--directory: (not as good) (ivy-state-current ivy-last)
             (setq dir (ivy-expand-file-if-directory (concat ivy--directory "/../"))))
        (ivy--cd dir)
        (ivy--exhibit))))

  (defun night/ivy--directory-enter ()
    (interactive)
    (ivy--directory-enter)
    )

  (define-key counsel-find-file-map (kbd "<left>")
    #'night/ivy--directory-out)
  (define-key counsel-find-file-map (kbd "<right>")
    #'night/ivy--directory-enter)
;;;
  ;; @ideal it's better to add these to the specific maps that need them, but I can't find what other map is used by, e.g., `read-file-name`
  (define-key ivy-minibuffer-map (kbd "<left>")
    #'night/ivy--directory-out)
  (define-key ivy-minibuffer-map (kbd "<right>")
    #'night/ivy--directory-enter)

  (define-key ivy-minibuffer-map (kbd "S-<right>") 'forward-char)
  (define-key ivy-minibuffer-map (kbd "S-<left>") 'backward-char)
;;;
  (defun night/ivy-halfpage-up ()
    (interactive)
    (ivy-previous-line 10)
    (minibuffer-recenter-top-bottom nil))

  (defun night/ivy-halfpage-down ()
    (interactive)
    (ivy-next-line 10)
    (minibuffer-recenter-top-bottom nil))

  (define-key ivy-minibuffer-map (kbd "M-<up>") #'night/ivy-halfpage-up)
  (define-key ivy-minibuffer-map (kbd "M-<down>") #'night/ivy-halfpage-down)
;;;
  (defun night/ivy-mark-toggle ()
    "Mark/unmark the selected candidate."
    (interactive)
    (let
        ((s (ivy-state-current ivy-last)))
      (when s (if (ivy--marked-p)
                  (ivy--unmark s)
                (ivy--mark s)))))

  (defun night/ivy-mark-toggle-up ()
    (interactive)
    (night/ivy-mark-toggle)
    (ivy-previous-line))
  (defun night/ivy-mark-toggle-down ()
    (interactive)
    (night/ivy-mark-toggle)
    (ivy-next-line))
  ;; @rememberMe
  (define-key ivy-minibuffer-map (kbd "TAB") #'night/ivy-mark-toggle)
  (define-key ivy-minibuffer-map (kbd "S-<up>") 'night/ivy-mark-toggle-up)
  (define-key ivy-minibuffer-map (kbd "S-<down>") 'night/ivy-mark-toggle-down)
  ;; (define-key ivy-minibuffer-map (kbd "S-TAB") 'ivy-unmark)
  ;; (define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-unmark)

  (defun night/ivy-set-to-sel ()
    (interactive)
    (let ((killed-line))
      (while (and
              (not (save-excursion (search-backward "/" (line-beginning-position) t)))
              (not (= 1 (line-number-at-pos (point))))
              (not (= (line-beginning-position) (point))))
        ;; fix for when ivy's prompt becomes so long that it goes to the next line (ivy hard wraps its prompt)
        (kill-whole-line)
        (setq killed-line t))
      (save-excursion (when (not (eq ?/ (char-before)))
                        (ignore-errors (zap-up-to-char -1 ?/))))

      (insert (ivy-state-current ivy-last))
      (when killed-line
        (save-excursion (insert "\n"))
        ;; we need to compensate for the lines we have killed
        )))
  (define-key ivy-minibuffer-map (kbd "M-<right>") #'night/ivy-set-to-sel)
;;;
  (defun night/ivy-show-doc-buffer ()
    "@broken"
    (interactive)
;;;
    ;; does not work
    ;; (info-lookup 'symbol (ivy-state-current ivy-last) 'lisp-mode)
;;; some code snippets you might useful when trying to fix this:
    ;; from /Users/evar/.emacs.d.doom/.local/straight/repos/company-mode/company-capf.el:142 :
    ;; (let ((f (plist-get (nthcdr 4 company-capf--current-completion-data)
    ;;                   :company-doc-buffer)))
    ;; (when f (funcall f arg)))
;;;
    ;; @todo doesn't work because the major mode is wrong in the counsel buffer (I think)
    ;; the key is getting the correct `company-capf--current-completion-data'
    (let ((cb (current-buffer))) ;; @idk how to get the main buffer
      (with-current-buffer cb
        (+lookup/documentation (ivy-state-current ivy-last))))
;;;
    ;; @todo doesn't work because of https://github.com/abo-abo/swiper/issues/2072#issuecomment-841639391
    ;; (let ((other-window-scroll-buffer))
    ;;   (progn
    ;;     (let* ((selected (ivy-state-current ivy-last))
    ;;            (doc-buffer (or (company-call-backend 'doc-buffer selected)
    ;;                            (user-error "No documentation available")))
    ;;            start)
    ;;       (when (consp doc-buffer)
    ;;         (setq start (cdr doc-buffer)
    ;;               doc-buffer (car doc-buffer)))
    ;;       (setq other-window-scroll-buffer (get-buffer doc-buffer))
    ;;       (let ((win (display-buffer doc-buffer t)))
    ;;         (set-window-start win (if start start (point-min)))))))
    )

  (defun night/ivy-sly-doc-popup ()
    (interactive)
    (let ((s (ivy-state-current ivy-last)))
      (message "ivy-doc s: %s" s)
      ;; (popup-tip "This is a tooltip.")
      (popup-tip (sly-eval `(slynk:describe-symbol ,s)))
      ;; (night/popup-sly-describe-symbol
      ;;  (i s))
      ))

  (define-key counsel-company-map (kbd "C-k") #'night/ivy-sly-doc-popup) ;; it might be worth it to activate this for commonlisp. It's not like the keybindings realestate in counsel-company-map is being used at all.

  ;; (define-key counsel-company-map (kbd "C-k") #'night/ivy-show-doc-buffer)

;;;
  (defun night/popup-sly-describe-symbol (symbol-name)
    "Popup function- or symbol-documentation for SYMBOL-NAME."
    ;; @todo0 make this work with counsel-company?
    (interactive (list (sly-read-symbol-name "Documentation for symbol: ")))
    (when (not symbol-name)
      (error "No symbol given"))
    (sly-eval-async `(slynk:describe-symbol ,symbol-name) 'popup-tip))

;;;

  (ignore-errors (memoize-restore #'night/ivy-docstring))
  (defun night/ivy-docstring (candidate)
    ;; (z bello)
    (let* (
           (candidate-sym (intern-soft candidate))
           (doc (cond
                 ((equalp major-mode 'emacs-lisp-mode)
                  (or
                   (ignore-errors
                     (helpful--docstring candidate-sym (fboundp candidate-sym))
                     ;; (helpful-symbol candidate-sym)
                     )
                   "")
                  )
                 ((equalp major-mode 'lisp-mode)
                  ;; (sly-eval `(slynk:describe-function ,candidate))
                  (let* ((doc
                          (sly-eval `(slynk:describe-symbol ,candidate))))
                    (night/brishz "sly-doc-oneline" (i doc)) ;; @futureCron @todo1 @perf works but very slow
                    ;; doc
                    ))
                 ((equalp major-mode 'sh-mode)
                  (night/brishz "wh-docstring" (i candidate))
                  ;; @futureCron is the slowdown worth it?
                  ;; I think it should be possible to speed this up, but I don't really know what's the bottleneck. Perhaps if ivy-rich did this async ...
                  )
                 (t "Not implemented yet") ;; search for `:company-doc-buffer' to see how packages handle company's documentation/help
                 ))
           (doc (s-lines doc))
           (doc (s-join " ; " (or doc '("")))))
      doc))

  (ignore-errors (memoize-restore #'night/ivy-docstring))
  (memoize #'night/ivy-docstring "999 hours")
;;; tests:
  (comment
   (night/ivy-docstring "printskskska8ss0")
   )
;;;

  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list 'counsel-company
                   '(:columns (
                               (ivy-rich-candidate (:width 0.4))
                               (night/ivy-docstring (:face font-lock-doc-face))))))
;;;
  )

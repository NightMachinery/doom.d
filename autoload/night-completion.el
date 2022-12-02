;;; ~/doom.d/autoload/night-completion.el -*- lexical-binding: t; -*-
;; * Some startup hooks set completion options.
;; ** [help:night/zsh-mode-startup]
;;;
(use-package! company

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . #'company-manual-begin)
         ([remap complete-symbol] . #'company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         :map company-active-map

         ;; Make TAB always complete the current selection, instead of
         ;; only completing a common prefix.
         ;; ("<tab>" . #'company-complete-selection)
         ;; ("TAB" . #'company-complete-selection)

         ;; C-s was bound to company-search-candidates, which is pretty useless. I don't know what benefits unbinding it has, but it doesn't hurt.
         ("C-s" . nil)

         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company. Note that
         ;; `:map' from above is "sticky", and applies also below: see
         ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.

         :filter (company-explicit-action-p)

         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company, instead of always
         ;; doing so.
         ("<return>" . #'company-complete-selection)
         ("RET" . #'company-complete-selection)

         ;; We then make <up> and <down> abort the completions menu
         ;; unless the user has interacted explicitly. Note that we
         ;; use `company-select-previous' instead of
         ;; `company-select-previous-or-abort'. I think the former
         ;; makes more sense since the general idea of this `company'
         ;; configuration is to decide whether or not to steal
         ;; keypresses based on whether the user has explicitly
         ;; interacted with `company', not based on the number of
         ;; candidates.
         ;;
         ;; Note that M-p and M-n work regardless of whether explicit
         ;; interaction has happened yet, and note also that M-TAB
         ;; when the completions menu is open counts as an
         ;; interaction.
         ("<up>" . #'company-select-previous)
         ("<down>" . #'company-select-next)

         ("S-<up>" . #'scroll-other-window-down)
         ("S-<down>" . #'scroll-other-window)
         )

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . #'company-manual-begin)
          ;; ("M-TAB" . #'counsel-company)
          ("<prior>" . #'counsel-company) ;; PgUp
          )

  :config

  (setq company-global-modes '(not erc-mode message-mode help-mode gud-mode circe-mode)) ;; @overrides/doom
  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  (setq company-frontends nil)
  ;; seems to be overridden by Doom?
  ;; I am now using this function to set the frontends. [help:night/company-frontends-default]

  (setq company-backends '(company-capf company-dabbrev-code company-yasnippet)) ; probably useless here, gets overridden


  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  ;; (setq company-require-match #'company-explicit-action-p)

  ;; Company appears to override our settings in `company-active-map'
  ;; based on `company-auto-complete-chars'. Turning it off ensures we
  ;; have full control.
  (setq company-auto-complete-chars nil)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  ;; (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  ;; (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t))

;;;

;;; Doesn't work, gets overrided by sth (prob doom)
;; (defun night/set-general-completion-backends()
;;     (interactive)
;;   (set (make-local-variable 'company-backends)
;;        '((
;;           ;; company-shell
;;           company-files                 ; files & directory
;;           company-keywords              ; keywords
;;           ;; company-capf
;;           company-yasnippet
;;           ;; company-abbrev
;;           company-dabbrev-code
;;           ))
;;        ))
(progn (setq +company-backend-alist
             '(
               ;; (julia-mode
               ;;  company-dabbrev-code ; default
               ;;  )
               ;;; useless here, gets overridden
               ;; (sh-mode
               ;;  ;; company-shell
               ;;  company-dabbrev-code
               ;;  company-files           ; files & directory
               ;;  company-keywords        ; keywords
               ;;  ;; company-capf
               ;;  company-yasnippet
               ;;  ;; company-abbrev
               ;;  )
               (org-mode
                ;; Also see `night/org-company-backends-set'
                company-capf company-files (company-dabbrev
                                            ;; company-ispell (makes org-babel's candidates become lost among the noise)
                                            ) company-yasnippet)
               (text-mode company-capf company-files (company-dabbrev company-ispell) company-yasnippet)
               (prog-mode company-capf company-dabbrev-code company-files company-yasnippet)
               (gerbil-mode company-etags company-capf company-dabbrev-code company-files company-yasnippet)
               (lua-mode company-capf company-dabbrev-code company-files company-yasnippet)
               (conf-mode company-capf company-dabbrev-code company-files company-yasnippet))))
(set-company-backend! 'sh-mode #'company-dabbrev-code) ; useless here, gets overridden

(defun night/org-company-backends-set ()
  (set-company-backend! 'org-mode
    '(company-capf company-files company-dabbrev company-yasnippet)))
;;;
(defun night/simple-completions ()
  (interactive)
  (setq company-backends '(company-capf company-files (company-dabbrev-code company-keywords) company-yasnippet))
  )
(defun night/text-completions ()
  (interactive)
  (setq company-backends '(company-capf company-files (company-dabbrev company-ispell) company-yasnippet))
  )
(defun tmp/test-completions ()
  (interactive)
  (setq company-backends '(company-capf company-files (company-dabbrev company-ispell)))
  )
;;;
;; https://stackoverflow.com/questions/2087225/about-the-fix-for-the-interference-between-company-mode-and-yasnippet
(after! lui
  (setq lui-completion-function #'night/company-yasnippet-or-completion))

(map!
 ;; :map undo-fu-mode-map
 :nviog
 ;; C-/ seems to type C-_ on my config -_-
 "C-_" #'night/company-yasnippet-or-completion
 "C-/" #'night/company-yasnippet-or-completion)

(defun night/company-yasnippet-or-completion ()
  (interactive)
  (let ((yas-fallback-behavior nil))
    (unless (yas-expand)
      ;; (call-interactively #'company-complete-common-or-cycle)
      (call-interactively #'counsel-company)
      )))

(defvar counsel-company-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'ivy-next-line)
    (define-key map (kbd "<tab>") #'ivy-next-line)
    map))
(after! (ivy counsel)
  (defun counsel-company ()
    "Complete using `company-candidates'."
    (interactive)

    (when company-mode (unless company-candidates
                         (company-complete))
          (let ((len (cond ((let (l)
                              (and company-common
                                   (string= company-common
                                            (buffer-substring
                                             (- (point) (setq l (length company-common)))
                                             (point)))
                                   l)))
                           (company-prefix
                            (length company-prefix)))))
            (when len
              (setq ivy-completion-beg (- (point) len))
              (setq ivy-completion-end (point))
              (cond
               ((= (length company-candidates) 1)
                (ivy-completion-in-region-action (car company-candidates))
                )
               (t
                ;; (message "candidate length %s" (length company-candidates))
                (ivy-read "Candidate: " company-candidates
                          :action #'ivy-completion-in-region-action
                          :caller 'counsel-company
                          :keymap counsel-company-map
                          ))))))))
;;;
;; C-z is overridden by others, use M-x for now
;; (after! company-try-hard
;;   (global-set-key (kbd "C-z") #'company-try-hard)
;;   (define-key company-active-map (kbd "C-z") #'company-try-hard))
;;;
;; company-quickhelp-terminal-mode was too buggy to use
;; (require 'company-quickhelp)
;; (company-quickhelp-mode)
;; (with-eval-after-load 'company-quickhelp
;;   (company-quickhelp-terminal-mode 1))

;; (defun night/completion-begin ()
;;   (interactive)
;;   (company-cancel)
;;   (company-manual-begin)
;;   )

;; (map!
;;  :nviog
;;  "C-j" #'night/completion-begin
;;  ;; does NOT work with quickhelp, but manually invoking it via M-x does
;;  )
;;;
;; the following will make a soft-wrapped doc popup show when browsing the completion candidates; Note that we do not need to be able to jump to this buffer, as we can just accept the completion and use =+lookup/documentation= on it. It would be nice to have a direct shortcut though.
;;
;; @todo0 use =scroll-other-window= instead?
;;
;; @helpme `company-doc-buffer` hides itself when I press any keys. How do I disable this autohiding? It makes it impossible to jump to the doc buffer when needed, and dismissing the window with ESC is easy enough anyway.

(defun night/company-doc-on-browsing-enable ()
  "Advises `company-select-next' et al to show the doc buffer."
  (interactive)
  (advice-add #'company-select-previous :after #'company-show-doc-buffer)
  (advice-add #'company-select-next :after #'company-show-doc-buffer))

(defun night/company-doc-on-browsing-disable ()
  "Removes all advices on `company-select-next' et al."
  (interactive)
  (night/unadvice 'company-select-previous)
  (night/unadvice 'company-select-next))

(when (not (display-graphic-p))
  (night/company-doc-on-browsing-enable))

(map!
 :map company-active-map
 :ig
 ;; "C-l" #'evil-window-next ;; @broken dismisses the popup, we probably need a doom-aware function
 ;; > I want a function that focuses the popup buffer. I tried using evil-next-window, but this closes the popup before being able to select it.
 )

(defun company-show-doc-buffer (&rest dummy) ;; @monkeyPatched to make it usable with the advices
  "Temporarily show the documentation buffer for the selection."
  (interactive)
  (let ((other-window-scroll-buffer)
        (selection (or company-selection 0)))
    (company--electric-do
      (let* ((selected (nth selection company-candidates))
             (doc-buffer (or (company-call-backend 'doc-buffer selected)
                             (user-error "No documentation available")))
             start)
        (when (consp doc-buffer)
          (setq start (cdr doc-buffer)
                doc-buffer (car doc-buffer)))
        (setq other-window-scroll-buffer (get-buffer doc-buffer))

;;; @monkeyPatched
        (with-current-buffer doc-buffer
          (night/wrap-soft-enable)

          ;; (message "company-doc-buffer-name: %s" doc-buffer) ;; usually "*Help*"
          )
;;;


        (let ((win (display-buffer doc-buffer t)))
          (set-window-start win (if start start (point-min))))))))
;;;
(defun night/disable-company ()
  (interactive)
  (company-mode -1))

(defun night/disable-company-frontends ()
  (interactive)
  (make-local-variable 'company-frontends)
  ;;; @experimental
  ;; (make-local-variable 'company-active-map)
  ;; (setq company-active-map (make-sparse-keymap))
  ;;;
  (setq company-frontends nil))

(defun night/enable-company-frontends ()
  (interactive)
  (kill-local-variable 'company-frontends))

(defalias 'night/company-frontends-default
  #'night/disable-company-frontends
  ;;;
  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  ;; (setq company-frontends '(company-pseudo-tooltip-frontenda))
  ;;;
  (comment
   (when (display-graphic-p)
     ;; (add-hook 'company-mode-hook 'company-box-mode)
     (company-box-mode)
     ))
  )
(comment
 (remove-hook 'company-mode-hook #'company-box-mode))
(add-hook 'text-mode-hook #'night/company-frontends-default)
(add-hook 'prog-mode-hook #'night/company-frontends-default)
;;;
(autoload 'helm-company "helm-company")
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))
;;;
(provide 'night-completion)

;;; ~/doom.d/autoload/night-completion.el -*- lexical-binding: t; -*-

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

         ;; When was the last time you used the C-s binding for
         ;; searching candidates? It conflicts with buffer search,
         ;; anyway.
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
         ("<down>" . #'company-select-next))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . #'company-manual-begin))

  :config

  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))
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
  (setq company-tooltip-align-annotations t)
  )

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
(defun night/sh-hook-fn()
  (interactive)
  (set-company-backend! 'sh-mode '(company-dabbrev-code company-files))
  )
(add-hook 'sh-mode-hook #'night/sh-hook-fn)


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
               (org-mode company-capf company-dabbrev-code company-yasnippet)
               (text-mode company-dabbrev company-yasnippet company-ispell)
               (prog-mode company-capf company-dabbrev-code company-yasnippet)
               (conf-mode company-capf company-dabbrev-code company-yasnippet))))
(set-company-backend! 'sh-mode #'company-dabbrev-code) ; useless here, gets overridden
;;;

(provide 'night-completion)

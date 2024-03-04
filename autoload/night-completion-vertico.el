;;; autoload/night-completion-vertico.el -*- lexical-binding: t; -*-

(after! (ivy night-ivy vertico vertico-multiform night-consult dabbrev cape)
;;;
  (advice-add 'company-capf :override #'ignore)
  ;; We disable company-capf to reduce the overhead of company.
  ;; I only use [help:company-active-map] currently.
  ;; We should find an alternative and give rid of company completely.

  (setq ivy-do-completion-in-region nil)
  (cond
   (t
    (setq completion-in-region-function
          (lambda (&rest args)
            "Use `consult-completion-in-region' if Vertico is enabled.
            Otherwise use the default `completion--in-region' function."
            (apply (if vertico-mode
                       #'consult-completion-in-region
                     #'completion--in-region)
                   args))))
   (nil
    (setq completion-in-region-function
          #'ivy-completion-in-region)))

  (comment
   (consult-customize consult-completion-in-region
                      ;; :completion-styles (basic)
                      :cycle-threshold 1)
   ;; =:cycle-threshold= errored for me. I guess the docs in the source file are out-of-date.
   )
;;;
  ;; https://stackoverflow.com/questions/2087225/about-the-fix-for-the-interference-between-company-mode-and-yasnippet
  (defun night/yasnippet-or-completion ()
    (interactive)
    (let ((yas-fallback-behavior nil))
      (unless (yas-expand)
        ;; (call-interactively #'company-complete-common-or-cycle)
        ;; (call-interactively #'counsel-company)
        (call-interactively #'completion-at-point))))
  (defalias 'night/company-yasnippet-or-completion #'night/yasnippet-or-completion)

  (after! lui
    (setq lui-completion-function #'night/yasnippet-or-completion))
;;;
  (map!
   :nig
   "C-/"
   #'completion-at-point
   ;; #'complete-symbol
   ;; #'cape-dabbrev
   ;; #'night/company-yasnippet-or-completion

   :nig
   ;; C-/ seems to type C-_ on my config -_-
   "C-_"
   #'completion-at-point
   ;; #'cape-dabbrev

   :nig
   "s-."
   #'cape-dabbrev
   ;; #'cape-tex
   )
;;;
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; See the global value:
  ;; (default-value 'completion-at-point-functions)

  (setq-default completion-at-point-functions
                (list
                 #'cape-file

                 ;; @upstreamBug? The order of elements in `cape-dabbrev' is not the same as `dabbrev-expand'.
                 #'cape-dabbrev
                 #'cape-keyword

                 ;; `cape-capf-super' is experimental.
                 ;; [https://github.com/minad/cape#super-capf---merging-multiple-capfs]
                 ;; [https://github.com/minad/cape#capf-buster---cache-busting]
                 ;; (cape-capf-super
                 ;;  #'cape-dabbrev
                 ;;  ;; #'cape-dict
                 ;;  #'cape-keyword)

                 ;; #'cape-tex
                 ))

  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; `dabbrev-capf' is buggy for me on emacs 29.2.
  ;; (add-to-list 'completion-at-point-functions #'dabbrev-capf)

  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
;;;
  (defun night/prioritize-global-capf ()
    "Moves global capfs to the beginning of capf list, so that local capfs won't break the global capfs."
    (interactive)
    (setq-local
     completion-at-point-functions
     (cl-remove-duplicates (-filter
                            ;; Removing =t= to avoid using the global capfs to avoid repetition:
                            (lambda (x)
                              (not (equal x t)))
                            (-concat
                             (default-value 'completion-at-point-functions)
                             completion-at-point-functions)))))
;;;
  ;; [[https://github.com/minad/consult/discussions/956][Add the ability to change the initial query in `consult-completion-in-region` · minad/consult · Discussion #956]]
  (setf (alist-get #'consult-completion-in-region vertico-multiform-commands)
        (list (lambda (_)
                ;; (vertico-multiform-grid)
                ;; `vertico-multiform-grid' worked the first time I ran this code, but then it stopped working after a minute. I have no idea why.

                (insert " ")
                )))
;;;
  )

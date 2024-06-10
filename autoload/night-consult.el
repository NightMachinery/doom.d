;;; autoload/night-consult.el -*- lexical-binding: t; -*-

(require 'vertico)
(require 'vertico-multiform)
(require 'consult)
(after! (ivy night-ivy night-orderless vertico consult)
  (vertico-mode)
  (vertico-multiform-mode)
;;;
  (setq consult-async-split-style 'nil)
  ;; (setq consult-async-split-style 'perl)
  ;; (setq consult-async-split-style 'comma)

  (setq consult-async-min-input 2)
  (setq consult-async-refresh-delay 0) ;; 0 was buggy in previous commits
  ;; (setq consult-async-refresh-delay 0.01)
  (setq consult-async-input-throttle 0)
  (setq consult-async-input-debounce 0)
;;;
  (map! :map vertico-map
        :g "M-<down>" #'vertico-scroll-up
        :g "M-<up>" #'vertico-scroll-down
        )

  (map!
   :leader "bp"
   #'consult-project-buffer
   ;; #'counsel-projectile-switch-to-buffer

   :leader "bb"
   #'consult-buffer
   ;; #'+ivy/switch-buffer

   :leader "sp"
   ;; #'+default/search-project
   ;; #'consult-ripgrep
   #'night/search-project

   :leader "sj"
   ;; #'+ivy/jump-list
   #'evil-collection-consult-jump-list
   )
;;;
  ;; [[https://github.com/minad/consult/discussions/947#discussioncomment-8565359][✏️ Draft - {Q} How do I hide the line numbers in, e.g., `consult-outline`? · minad/consult · Discussion #947]]
  (consult-customize consult-outline :annotate nil)
  (consult-customize consult-line :annotate nil)
;;;
  (defun night/consult-line-all-buffers (&optional initial-query)
    (interactive)
    (consult-line-multi t initial-query))
;;;
  ;; [[https://github.com/minad/consult/wiki#pre-select-nearest-heading-for-consult-org-heading-and-consult-outline-using-vertico][Home · minad/consult Wiki]]
  ;; @works [[https://github.com/minad/consult/discussions/891][wiki code for pre-selecting nearest heading erroring out vertico--exhibit wrong-type-argument · minad/consult · Discussion #891]]
  (defvar consult--previous-point nil
    "Location of point before entering minibuffer.
Used to preselect nearest headings and imenu items.")

  (defvar vertico--previous-input nil
    "Previous vertico input so we can distinguish whether user is changing input string.")

  (defun consult--set-previous-point (&rest _)
    "Save location of point. Used before entering the minibuffer."
    (setq vertico--previous-input nil)
    (setq consult--previous-point (point)))

  (advice-add #'consult-org-heading :before #'consult--set-previous-point)
  (advice-add #'consult-outline :before #'consult--set-previous-point)


  (advice-add #'vertico--update :after #'consult-vertico--update-choose)

  (defun consult-vertico--update-choose (&rest _)
    "Pick the nearest candidate rather than the first after updating candidates."
    ;; we only select the closest heading if the user is changing filter string
    ;; this happens at invocation, and as the user filters
    ;; as they filter, we want to keep on selecting (if possible) the heading they were closest to
    (when (and (memq current-minibuffer-command
                     '(consult-org-heading consult-outline))
               (not (equal vertico--input vertico--previous-input)))
      (setq vertico--previous-input (copy-tree vertico--input))
      (setq vertico--index
            (max 0                      ; if none above, choose the first below
                 (1- (or (seq-position
                          vertico--candidates
                          consult--previous-point
                          (lambda (cand point-pos) ; counts on candidate list being sorted
                            (> (cl-case current-minibuffer-command
                                 (consult-outline
                                  (car (consult--get-location cand)))
                                 (consult-org-heading
                                  ;; cpbotha's work-around, see https://github.com/minad/consult/discussions/891
                                  (get-text-property 0 'org-marker cand)))
                               point-pos)))
                         (length vertico--candidates)))))))
;;;
  ;; [[id:448bea31-bdbc-4f75-ae6c-97b4d69ec00c][Hide some buffers]]
  (setq consult-buffer-filter
        ;; nil
        '("\\` "
          "\\`\\*\\(Async-\\)?\\([nN]ative-\\)?[cC]ompile-[lL]og\\*\\'"
          "\\`\\*brishz-err\\*\\'"
          "\\`\\*doom\\*\\'"
          "\\`\\*Completions\\*\\'"
          "\\`\\*Flymake log\\*\\'"
          "\\`\\*Semantic SymRef\\*\\'"
          "\\`\\*tramp/.*\\*\\'")
        )
;;;
  (cond
   (t
    (dolist (fn
             (list
              #'consult--read
              ;; #'read-file-name
              ))
      (advice-add
       fn
       :around
       (lambda (&rest app)
         (let ((completing-read-function #'completing-read-default))
           (apply app))))))
   (t
    (dolist (fn
             '(consult-line
               consult-yank-pop
               consult-yank-from-kill-ring

               consult-buffer
               ;; [[id:3f702604-a1ae-44aa-85fa-750080f0cc50][consult/buffer]]

               consult-project-buffer
               consult-bookmark
               consult-fd
               consult-complex-command

               consult-imenu
               consult-outline
               consult-org-heading
               ;; like `counsel-outline', but with colors

               consult-grep
               consult--grep
               consult-ripgrep
               night/search-dir-consult
               night/search-notes
               night/agsi
               ))
      (add-to-list 'ivy-completing-read-handlers-alist
                   `(,fn . completing-read-default)))))
  (dolist (fn
           '(
             execute-extended-command
             ))
    (add-to-list 'ivy-completing-read-handlers-alist
                 `(,fn . completing-read-default)))
;;;
  (provide 'night-consult)
  )

;;; autoload/evil/night-text-objects.el -*- lexical-binding: t; -*-
(use-package! exato                     ;; xml attributes text-object
  ;; :ensure t
  :init
  (setq exato-key "x"))

(after! targets
  ;; @docs https://github.com/noctuid/targets.el#i-a-i-a-l-n-r
  ;; this is very cool, but unfortunately, targets is buggy and its advanced features (such as =v i r f= don't work well). https://github.com/noctuid/targets.el/issues/32

  (targets-setup)
  ;; (targets-setup t) ;; binds its default text objects

  (targets-define-to pipe "|" nil separator
                     :bind t :keys "|")

  (targets-define-to shfun
                     ;; "function "
                     "function .*\(?\)?\s+\{" ;; this makes the inner text objects work correctly, but it breaks the remote, next, last text objects
                     "^}\n" pair
                     :bind t :hooks (sh-mode-hook) :keys "f")

  ;; (defun targets--shrink-inner (bounds)
  ;;   "Shrink RANGE by 1 character on each side."
  ;;   (cl-incf (car bounds))
  ;;   (cl-decf (cadr bounds))
  ;;   bounds)
  ;; (put 'my-thing 'targets-shrink-inner-op #'targets--shrink-inner)

  )

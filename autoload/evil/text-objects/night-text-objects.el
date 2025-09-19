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
  (targets-define-to triple-quotes "\\(?:\"\\|'\\)\\{3\\}.*" nil separator
                     :bind t :keys "B")

  (targets-define-to comment-cell "\\(?:#\\{2,\\}\\|;\\{3,\\}\\).*" nil separator
                     :bind t :keys "c")  ;; @seeAlso `night/cell-select'

  (targets-define-to shfun
                     ;; "function "
                     "function .*\(?\)?\s+\{" ;; this makes the inner text objects work correctly, but it breaks the remote, next, last text objects
                     "^}\n" pair
                     :bind t :hooks (sh-mode-hook) :keys "f")
  ;; (symbol-function 'targets-inner-shfun)

;;; this doesn't work when you're at the end of the line:
  ;; (targets-define-to line
  ;;                    "^\s*" ;; this makes the inner text objects work correctly, but it breaks the remote, next, last text objects
  ;;                    "\s*$" pair
  ;;                    :bind t :keys "l")
;;;

  ;; (defun targets--shrink-inner (bounds)
  ;;   "Shrink RANGE by 1 character on each side."
  ;;   (cl-incf (car bounds))
  ;;   (cl-decf (cadr bounds))
  ;;   bounds)
  ;; (put 'my-thing 'targets-shrink-inner-op #'targets--shrink-inner)

  )

(comment

 (defmacro define-and-bind-text-object (key start-regex end-regex)
   (let ((inner-name (make-symbol "inner-name"))
         (outer-name (make-symbol "outer-name")))
     `(progn
        (evil-define-text-object ,inner-name (count &optional beg end type)
          (evil-select-paren ,start-regex ,end-regex beg end type count nil))
        (evil-define-text-object ,outer-name (count &optional beg end type)
          (evil-select-paren ,start-regex ,end-regex beg end type count t))
        (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
        (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

 ;; create "il"/"al" (inside/around) line text objects:
 (define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
 )

(after! evil
  ;; @forked from https://github.com/emacsorphanage/evil-textobj-line
  ;;;
  (defgroup evil-textobj-line nil
    "Text object line for Evil"
    :group 'evil)

  (defcustom evil-textobj-line-i-key ";"
    "Keys for evil-inner-line"
    :type 'string
    :group 'evil-textobj-line)

  (defcustom evil-textobj-line-a-key ";"
    "Keys for evil-a-line"
    :type 'string
    :group 'evil-textobj-line)

  (defun evil-line-range (count beg end type &optional inclusive)
    (if inclusive
        (evil-range (line-beginning-position) (line-end-position))
      (let ((start (save-excursion
                     (back-to-indentation)
                     (point)))
            (end (save-excursion
                   (goto-char (line-end-position))
                   (skip-syntax-backward " " (line-beginning-position))
                   (point))))
        (evil-range start end))))

  (evil-define-text-object evil-a-line (count &optional beg end type)
    "Select range between a character by which the command is followed."
    (evil-line-range count beg end type t))
  (evil-define-text-object evil-inner-line (count &optional beg end type)
    "Select inner range between a character by which the command is followed."
    (evil-line-range count beg end type))

  (define-key evil-outer-text-objects-map evil-textobj-line-a-key 'evil-a-line)
  (define-key evil-inner-text-objects-map evil-textobj-line-i-key 'evil-inner-line)

  (map!
   :textobj "C" #'evilnc-inner-comment #'evilnc-outer-commenter)
  (provide 'evil-textobj-line))

;;; night-todo-states.el ---                         -*- lexical-binding: t; -*-
;;; Code:
(defun night/modify-org-done-face ()
  (interactive)
  (setq org-fontify-done-headline nil)
  (set-face-attribute 'org-done nil :strike-through nil)
  (comment
   (set-face-attribute 'org-headline-done nil
                       ;; :strike-through "black" ; doesn't work for me
                       :strike-through nil
                       :foreground "cadetblue"
                       )))

(defun night/org-ui-todo-setup ()
  (interactive)
  (night/modify-org-done-face)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "LOOP" "STRT(s)" "WAIT(w)" "|" "DONE(d)" "KILL(k)")
          (sequence
           "@idea(i)"
           "IDEA"
           "@idea/costly(e)"
           "@idea/rejected(r)"
           "@idea/presented(p)"
           "@idea/bad(b)"
           "|" "@idea/accepted(=)")
          ;; (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
          (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")

          (sequence "QUESTION(q)" "CONFIRM(c)" "TANGENT(g)" "|" "ANSWERED(a)")))

  (defface night/question-face '((t (
;;;
                                     ;; :foreground "white"
                                     ;; :background "blue3"
;;;
                                     ;; :foreground "blue3"
                                     :foreground "cornflowerblue"
                                     :weight bold))) "")
  (setq org-todo-keyword-faces
        '(("[-]" . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]" . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO" . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)

          ("QUESTION" . night/question-face)
          ("CONFIRM" . +org-todo-onhold)
          ("TANGENT" . +org-todo-active)))
  (provide 'night-todo-states))

(night/org-ui-todo-setup)
(add-hook! 'org-load-hook :append #'night/org-ui-todo-setup) ;; doom will add some other hooks which will be run before ours
;;; night-todo-states.el ends here

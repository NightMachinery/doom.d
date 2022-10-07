;;; night-todo-states.el ---                         -*- lexical-binding: t; -*-
;;; Code:

(after! (org)
 (setq org-todo-keywords
       '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
         ;; (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
         (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")

         (sequence "QUESTION(q)" "CONFIRM(c)" "TANGENT(g)" "|" "ANSWERED(a)")
         )))

(setq org-todo-keyword-faces
      '(("[-]" . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]" . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("PROJ" . +org-todo-project)
        ("NO" . +org-todo-cancel)
        ("KILL" . +org-todo-cancel)

        ("CONFIRM" . +org-todo-onhold)
        ("TANGENT" . +org-todo-active)
        ))

(provide 'night-todo-states)
;;; night-todo-states.el ends here

;;; autoload/org/night-extract.el -*- lexical-binding: t; -*-

(after! (org evil-org)
  (defun night/org-todo-copy ()
    (interactive)
    (let*
        ((headings (org-element-map (org-element-parse-buffer) 'headline #'identity))
         (todo-headings
          (-filter
           (lambda (heading)
             (let ((todo-type (org-element-property :todo-type heading)))
               (comment
                (message "todo-type: %S" todo-type)
                (when todo-type
                  (setq a todo-type)))
               (equalp todo-type 'todo)))
           headings))
         (texts
          (-map (lambda (heading)
                  (org-element-property :raw-value heading))
                todo-headings))
         (text-all (s-join "
" texts)))
      (kill-new text-all))))

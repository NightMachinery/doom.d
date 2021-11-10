;;; tmp.el -*- lexical-binding: t; -*-
;;;
(signal 'error (list "you're attempting to eval tmp.el; aborted"))
(z bello)
;;;
(setq glyphless-char-display (make-display-table))
;;;
(setq *night/org-babel-remote* :f1)
(setq *night/org-babel-remote* nil)
(setq *night/org-babel-remote* :remote_notebook_server)
;;;
(add-to-list 'tramp-remote-path "/home/ubuntu/miniconda3/bin/")

(setq tramp-default-remote-shell "/bin/my.sh")
(add-to-list 'tramp-remote-process-environment "PYTHONPATH='/home/ubuntu/miniconda3/lib/python3.9/site-packages'")

(dired "/jpy:51.178.215.202#2390:/")
;;;
(format "hi %s" "ji")
(s-join "\n" (list "hi" "jo"))
;;;

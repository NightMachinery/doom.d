;;; night-agent-desk.el --- agent-desk commands in its rendered org files  -*- lexical-binding: t; -*-
;;
;; The package lives in the agent-desk repository; hosts without a clone skip it.
;; Keys in desk files: a answer, t take, r reply, d read, U unread, p pin,
;; o open, gr refresh (evil normal state); the same under <localleader> j.

(let ((night/h-agent-desk-el (expand-file-name "~/code/skills/agent-desk/emacs/agent-desk.el")))
  (when (file-exists-p night/h-agent-desk-el)
    (load night/h-agent-desk-el nil t)
    (map! :map agent-desk-org-mode-map
          :localleader
          (:prefix ("j" . "agent-desk")
           "a" #'agent-desk-answer
           "t" #'agent-desk-take
           "r" #'agent-desk-reply
           "d" #'agent-desk-read
           "U" #'agent-desk-unread
           "p" #'agent-desk-pin
           "o" #'agent-desk-open
           "y" #'agent-desk-copy-id
           "g" #'agent-desk-refresh
           "i" #'agent-desk-inbox))))

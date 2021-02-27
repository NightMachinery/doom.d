;;; ~/doom.d/night-ssh.el -*- lexical-binding: t; -*-

(defun night/ssh-pre ()
  (interactive)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  ;; Bug in Spacemacs https://github.com/syl20bnr/spacemacs/issues/11514
  ;; (remove-hook 'python-mode-hook 'spacemacs//init-eldoc-python-mode)
  )

(night/ssh-pre)

(defun night/tramp-refresh ()
  (interactive)
  (recentf-cleanup)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))
(defun night/scp-eva-borg ()
  (interactive)
  (find-file-existing "/scp:eva@82.102.11.148:/home/eva/code/betterborg/stdplugins/"))
(defun night/ssh-eva ()
  (interactive)
  (dired "/ssh:eva@82.102.11.148:/home/eva/scripts/"))
(defun night/scp-eva ()
  (interactive)
  (dired "/scp:eva@82.102.11.148:/home/eva/scripts/"))
(defun night/scp-zii ()
  (interactive)
  (dired "/scp:zii@51.178.215.202:/home/zii/"))
(defun night/ssh-eva-current ()
  (interactive)
  ;; s-replace from s.el
  (find-file-existing (concat "/ssh:eva@82.102.11.148:" (s-replace (getenv "HOME") "/home/eva" (s-replace "/Users/evar/Base/_Code" "/home/eva/code" (buffer-file-name))))))

(defun night/scp-eva-current ()
  (interactive)
  ;; s-replace from s.el
  (find-file-existing (concat "/scp:eva@82.102.11.148:" (s-replace (getenv "HOME") "/home/eva" (s-replace "/Users/evar/Base/_Code" "/home/eva/code" (buffer-file-name))))))

(defun night/webdav-eva-current ()
  (interactive)
  ;; s-replace from s.el
  (find-file-existing (concat (s-replace (getenv "HOME") "/Volumes" (buffer-file-name))))
  (message "%s" (current-buffer)))

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=yes")

;;;
(map! :leader
      ;; remote
      "z r" #'night/scp-eva-current
      ;; "z r" #'night/ssh-eva-current
      ;; "z r" #'night/webdav-eva-current
      )

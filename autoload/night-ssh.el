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
  (remove-hook 'python-mode-hook 'spacemacs//init-eldoc-python-mode))
(defun night/tramp-refresh ()
  (interactive)
  (recentf-cleanup)
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))
(defun scp-borg-evil-lord ()
  (interactive)
  (find-file-existing "/scp:mary@198.143.181.104:/home/mary/code/uniborg/stdplugins/evil_lord.py"))
(defun ssh-eva ()
  (interactive)
  (dired "/ssh:eva@82.102.11.148:/home/eva/scripts/"))
(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=yes")

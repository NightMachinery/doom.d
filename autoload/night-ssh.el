;;; ~/doom.d/night-ssh.el -*- lexical-binding: t; -*-

(after! tramp-theme
  (setq tramp-theme-face-remapping-alist '(;; (nil "^root$" (fringe (:inherit fringe :inverse-video t)))
                                           (nil "^root$" (default (:background "lavenderblush")))
                                           (nil "^root$" (hl-line (:background "mistyrose") hl-line))
                                           (".*" "eva" (default (:background "mintcream")))
                                           (".*" "eva" (hl-line (:background "azure") hl-line))
                                           (".*" "zii" (default (:background "honeydew")))
                                           (".*" "zii" (hl-line (:background "azure") hl-line))
                                           ("^foo$" nil (dired-directory (:background "Red")))
                                           ("^foo$" nil (eshell-prompt (:foreground "White")))
                                           ("^bar$" nil (default (:background "Green")))
                                           ("^bar$" nil (dired-directory (:background "Green"))))
        )
  )
(defun night/load-tramp-theme ()
  (interactive)
  (load-theme 'tramp t t)
  ;; (load-theme night-theme t)
  (enable-theme 'tramp)
  ;; (enable-theme 'tramp)
)
;;;
(defun night/ssh-pre ()
  (interactive)
  ;; (night/load-tramp-theme)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (setq remote-file-name-inhibit-cache nil)
  ;;;
  ;; completely disabled anaconda-mode instead
  ;; (add-hook 'python-mode-hook #'turn-off-anaconda-eldoc-mode)
  ;;;
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
(defun night/ssh-eva-borg ()
  (interactive)
  (find-file-existing "/ssh:eva@82.102.11.148:/home/eva/code/betterborg/stdplugins/"))
(defun night/ssh-eva ()
  (interactive)
  (dired "/ssh:eva@82.102.11.148:/home/eva/scripts/"))
(defun night/scp-eva ()
  (interactive)
  (dired "/scp:eva@82.102.11.148:/home/eva/scripts/"))
;;;
;; https://emacs.stackexchange.com/questions/64090/change-company-backends-for-tramp-buffers
(defun night/tramp-enter ()
  (interactive)
  (night/simple-completions)
  ;; (enable-theme 'doom-one-light)
  )
(defun night/tramp-exit ()
  (interactive)
  ;; (disable-theme 'doom-one-light)
  )
;; This fired all the time, so it's unusable:
;; (add-hook
;;  'buffer-list-update-hook
;;  (lambda ()
;;    (if (and (not (window-minibuffer-p))
;;             (file-remote-p default-directory))
;;        (progn
;;          (message default dir: %s" default-directory)
;;          (night/tramp-enter))
;;      (night/tramp-exit))))
;;;
(defun night/scp-zii ()
  (interactive)
  (dired "/scp:zii@51.178.215.202:/home/zii/"))

(defun night/scp-behy-root ()
  (interactive)
  (dired "/scp:root@51.89.107.137:/root"))

(defun night/scp-behy-walle ()
  (interactive)
  (dired "/scp:walle@51.89.107.137:/home/walle/"))

(defun night/scp-zii-current ()
  (interactive)
  ;; s-replace from s.el
  (find-file-existing (concat "/scp:zii@51.178.215.202:" (s-replace (getenv "HOME") "/home/zii" (s-replace "/Users/evar/Base/_Code" "/home/zii/code" (buffer-file-name))))))
;;;
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

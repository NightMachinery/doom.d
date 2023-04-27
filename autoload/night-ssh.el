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
                                           ("^bar$" nil (dired-directory (:background "Green"))))))

(defun night/load-tramp-theme ()
  (interactive)
  (if (and
       ;; (not (night/server-alt1-p))
       ;; nil ;; the tramp theme is broken on emacs 28 (with or without native comp)(black text is rendered red and other fun stuff)(tested on MBP, Grayfur, and the old laptop)
       (zb isLocal)
       ;; (zb isMe)
       )
      (progn (load-theme 'tramp t t)
             ;; (custom-theme-enabled-p 'tramp)
             ;; (load-theme night-theme t)
             (enable-theme 'tramp)
             ;; (disable-theme 'tramp)
             (night/h-local-theme-workaround))))
;;;
(defun night/ssh-pre ()
  (interactive)
  ;; (night/load-tramp-theme)
  ;; https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
  (setq remote-file-name-inhibit-cache nil)

  (setq remote-file-name-inhibit-locks t)
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
  ;; (recentf-cleanup) ;; takes too long
  (tramp-cleanup-all-buffers)
  (tramp-cleanup-all-connections))
;;;
(defun night/scp-hpc1 ()
  (interactive)
  (find-file-existing "/scp:hpc1:/home/user01/code/uni/"))
;;;
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
(defun night/scp-pari ()
  (interactive)
  (dired "/scp:paria@Parias-MacBook-Air.local:/Users/paria/scripts/"))

(defun night/ssh-pari ()
  (interactive)
  (dired "/ssh:paria@Parias-MacBook-Air.local:/Users/paria/scripts/"))
;;;
(defun night/ssh-nm ()
  (interactive)
  (dired "/ssh:ubuntu@nightmachinery.ir:"))

(defun night/scp-nm ()
  (interactive)
  (dired "/scp:ubuntu@nightmachinery.ir:"))
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
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; This adds the -l flag to the shell command and sources ~/.profile
;;;
(defvar *night/org-babel-remote* nil)
(defun night/org-babel-session-name-get (&optional name)
  ;; @upstreamBug [[https://github.com/nnicandro/emacs-jupyter/issues/352][nnicandro/emacs-jupyter#352 Bug when using elisp to determine the session name]]
  (let ((name (or name "s1")))
    (cond
     ((equalp *night/org-babel-remote* :pari)
      (concat "/ssh:paria@Parias-MacBook-Air.local:" name))
     ((equalp *night/org-babel-remote* :remote_connection_file)
      (concat
       "/ssh:walle@51.178.215.202#2380:"
       ;; "/ssh:ubuntu@194.5.193.126:"
       ;; name
       "tmp/jupyter_kernels/" name ".json" ;; start the kernel manually
       ))
     ((equalp *night/org-babel-remote* :remote_notebook_server)
      (concat
       "/jpy:51.178.215.202#2390:/"
       name
       ))
     (t name))))
;;;
(map! :leader
      ;; remote
      "z r" #'night/scp-eva-current
      ;; "z r" #'night/ssh-eva-current
      ;; "z r" #'night/webdav-eva-current
      )

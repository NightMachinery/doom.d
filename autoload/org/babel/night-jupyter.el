;;; autoload/org/babel/night-jupyter.el -*- lexical-binding: t; -*-

(require 'org-src)
(require 'ob-async)
(require 'jupyter)
(require 'jupyter-org-client)

(after! (jupyter)
  (setq jupyter-long-timeout 20))
(after! (ob-async)
  (setq ob-async-no-async-languages-alist
        '("jupyter-python" "jupyter-julia")))

(after! (jupyter-org-client)
;;;
  (setq-default org-babel-default-header-args:jupyter-python nil)
  (add-to-list 'org-babel-default-header-args:jupyter-python '(:results . "scalar"))
;;;
  (defun night/jupyter-tramp-read-passwd (filename &optional prompt)
    "Read a password based off of FILENAME's TRAMP filename components.
Use PROMPT to prompt the user for the password if needed, PROMPT
defaults to \"Password:\"."
    (unless (jupyter-tramp-file-name-p filename)
      (error "Not a Jupyter filename"))
    (let ((prompt (or prompt "Password: ")))
      (comment
       (with-parsed-tramp-file-name filename nil
         (let ((tramp-current-method method)
               (tramp-current-user (or user user-login-name))
               (tramp-current-domain nil)
               (tramp-current-host host)
               (tramp-current-port port))
           (tramp-read-passwd nil prompt))))
      (password-read prompt filename)))
  (advice-add 'jupyter-tramp-read-passwd :override #'night/jupyter-tramp-read-passwd)
;;;
  (defun night/jupyter-forget-client-v1 (key)
    "Forget a Jupyter client session identified by KEY.
@seeAlso `night/counsel-jupyter-forget-client'
"
    (interactive "sEnter session key to forget: ")
    (remhash key org-babel-jupyter-session-clients)
    (message "Forgot session: %s" key))

  (defun night/jupyter-forget-client (key)
    "Forget a Jupyter client session identified by KEY.
Also kill the associated REPL and object buffers.
@seeAlso `night/counsel-jupyter-forget-client'"
    (interactive "sEnter session key to forget: ")
    (when-let* ((client (gethash key org-babel-jupyter-session-clients)))
      (let ((repl-buffer (oref client buffer)))
        ;; Kill the REPL buffer
        (when (buffer-live-p repl-buffer)
          ;; (kill-buffer repl-buffer)
          ;; If we use `kill-buffer', the kill hooks will be triggered which will ask us if we want to kill the kernel. We don't, we just want to clear the garbage and leave the kernel alone.
          (night/force-kill-buffer repl-buffer)
          )
        ;; Kill the object buffer
        (jupyter-with-client-buffer client
          (when (buffer-live-p (current-buffer))
            ;; (kill-buffer (current-buffer))
            (night/force-kill-current-buffer)
            )))

      ;; Remove the client from the `org-babel-jupyter-session-clients' hash table
      (remhash key org-babel-jupyter-session-clients)
      (message "Forgot session: %s" key)))

  (defun night/counsel-jupyter-forget-client ()
    "Interactively select and forget one or multiple Jupyter client sessions."
    (interactive)
    (let ((keys (hash-table-keys org-babel-jupyter-session-clients)))
      (ivy-read "Select sessions to forget: " keys
                :action (lambda (selected)
                          (night/jupyter-forget-client selected))
                :multi-action (lambda (selected)
                                (dolist (key selected)
                                  (night/jupyter-forget-client key)))
                :caller 'night/counsel-jupyter-forget-client)))
;;;
  (defun night/jupyter-org-eval-line-or-region ()
    (interactive)
    (jupyter-org-with-src-block-client
     (call-interactively #'jupyter-eval-line-or-region)))

  (defun night/jupyter-org-eval-defun ()
    (interactive)
    (jupyter-org-with-src-block-client
     (call-interactively #'jupyter-eval-defun)))

  (defun night/jupyter-org-inspect-at-point ()
    (interactive)
    (jupyter-org-with-src-block-client
     (call-interactively #'jupyter-inspect-at-point)))

  (map! :map 'jupyter-org-interaction-mode-map
        :localleader
        "ee" #'night/jupyter-org-eval-line-or-region
        "ed" #'night/jupyter-org-eval-defun

        ;; "i" #'night/jupyter-org-inspect-at-point
        ;; "i" conflicts with [help:org-toggle-item]

        "ki" #'jupyter-org-interrupt-kernel
        "kr" #'jupyter-repl-restart-kernel
        "kf" #'night/counsel-jupyter-forget-client

        "h" #'jupyter-org-hydra/body
        ))

;;; autoload/org/babel/night-jupyter.el -*- lexical-binding: t; -*-

(require 'org-src)
(require 'ob-async)
(require 'ob-jupyter)
(require 'jupyter)
(require 'jupyter-org-client)

(require 'zmq)
(zmq-load)

(after! (ob-jupyter)
  (org-babel-jupyter-aliases-from-kernelspecs))
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
    ;; (message "jupyter-password requested for: filename=%s prompt=%s" filename prompt)
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
      (password-read-redis prompt filename)))
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

    ;; Remove the client from the `org-babel-jupyter-session-clients' hash table
    (remhash key org-babel-jupyter-session-clients)

    (when-let* ((client (gethash key org-babel-jupyter-session-clients)))
      (let ((repl-buffer (oref client buffer)))
        (message "got repl-buffer")

        ;; Kill the REPL buffer
        (when t ;; (buffer-live-p repl-buffer)
          (message "killing repl-buffer ...")
          ;; (kill-buffer repl-buffer)
          ;; If we use `kill-buffer', the kill hooks will be triggered which will ask us if we want to kill the kernel. We don't, we just want to clear the garbage and leave the kernel alone.
          (night/force-kill-buffer repl-buffer))
        ;; Kill the object buffer
        (jupyter-with-client-buffer client
          (message "got client-buffer")
          (when t ;; (buffer-live-p (current-buffer))
            (message "killing client-buffer ...")
            ;; (kill-buffer (current-buffer))
            (night/force-kill-current-buffer))))

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
  (defun night/jupyter-python-session-get-current ()
    "Retrieve the Jupyter Python session from the current Org headline's property.
If the `jupyter-python-session` property is set in the current headline, this function returns its value. If not set, it defaults to '/jpy:127.0.0.1#6035:orgk1/'.

For example, you can set the session for the entire file by adding the following at the beginning of your Org file:
:PROPERTIES:
:jupyter-python-session: /jpy:127.0.0.1#7031:orgk1/
:END:

This function should be called within an Org-mode buffer."
    (interactive)
    (let* (
           (default "/jpy:127.0.0.1#6035:orgk1/")
           (session default)
           (session
            (cond
             ((derived-mode-p 'org-mode)
              (or
               (org-entry-get nil "jupyter-python-session" t)
               default))
             (t
              ;; (error "This function must be called in an Org-mode buffer")
              default))))
      (when (called-interactively-p 'any)
        (message "jupyter-python-session: %s" session))
      session))

  (defun night/jupyter-python-session-orgprop-set (session)
    "Set the `jupyter-python-session` property for the current Org headline.
SESSION should be a string representing the Jupyter session."
    (interactive "sEnter Jupyter session string: ")
    (org-set-property "jupyter-python-session" session))
;;;
  (defun night/search-for-jupyter-error ()
    "Search forward for Jupyter error patterns.
The pattern matches lines starting with : followed by any non-whitespace
characters and ending with 'Error:'."
    (interactive)
    (if (fboundp 'evil-mode)
        (cond
         (t
          (progn
            ;; Set the search direction to forward
            (setq evil-ex-search-direction 'forward)
            ;; Update the last search pattern
            (setq evil-ex-search-pattern
                  (let ((evil-ex-search-vim-style-regexp nil))
                    (evil-ex-make-search-pattern "^:\\s-\\S-+\\(Error\\|Exception\\):")))
            ;; Highlight all matches
            (evil-ex-search-activate-highlight evil-ex-search-pattern)
            ;; Move to the next occurrence
            (evil-ex-search-next 1)))
         (t
          (evil-search "^:\\s-\\S-+Error:" t t)))
      (message "evil-mode is not available. Please ensure evil is loaded.")))
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

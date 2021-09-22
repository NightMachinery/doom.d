;;; ~/doom.d/autoload/night-julia.el -*- lexical-binding: t; -*-

(after! eglot
  (setq eglot-connect-timeout 600)
  (eglot-jl-init)
  )
;; (setq lsp-julia-package-dir nil)

(defun night/julia-env-last-get ()
  (concat "~/.julia/environments/"
          (z reval-withstdin
             (z reval-withstdin
                (z ls (expand-file-name "~/.julia/environments/"))
                gsort --version-sort)
             gtail -1)))

(after! eglot-jl
  ;; (setq lsp-julia-default-environment "~/.julia/environments/v1.7")
  ;; (setq lsp-julia-default-environment (night/julia-env-last-get))
  (let ((base-env
         (or (ignore-errors (z ffz-get BaseEnv))
             "/") ;; @bootstrapMe Clone BaseEnv.jl and make sure ffz-get remembers its path
         ))
    (cond
     ((f-exists-p (concat base-env "/Project.toml"))
      (setq eglot-jl-language-server-project base-env))
     (t
      (message "@warn BaseEnv.jl not configured properly, falling back to eglot-jl's default env")
      (setq eglot-jl-language-server-project eglot-jl-base)))))

(comment
;;; froked from doom
 (use-package! julia-mode
   :interpreter "julia"
   :config
   (set-repl-handler! 'julia-mode #'+julia/open-repl))
;;;###autoload
 (defun +julia/open-repl ()
   "Run an inferior instance of `julia' inside Emacs."
   (interactive)
   (if (require 'julia-repl nil t)
       (prog1 (julia-repl)
         (julia-repl-use-emacsclient))
     (let ((buffer (get-buffer-create "*Julia*")))
       (unless (comint-check-proc "*Julia*")
         (apply #'make-comint-in-buffer "Julia" "*Julia*" julia-program julia-arguments))
       (pop-to-buffer buffer)
       (with-current-buffer buffer
         (inferior-julia-mode))
       buffer)))

 (use-package! julia-repl
   :preface (defvar +julia-repl-start-hook nil)
   :hook (julia-mode . julia-repl-mode)
   :hook (+julia-repl-start . +julia-override-repl-escape-char-h)
   :hook (+julia-repl-start . julia-repl-use-emacsclient)
   :config
   (set-popup-rule! "^\\*julia.*\\*$" :ttl nil :height 0.3)

   (when (featurep! :ui workspaces)
     (defadvice! +julia--namespace-repl-buffer-to-workspace-a (&optional executable-key suffix)
       "Name for a Julia REPL inferior buffer. Uses workspace name for doom emacs"
       :override #'julia-repl--inferior-buffer-name
       (concat julia-repl-inferior-buffer-name-base ":" (+workspace-current-name))))

   (defadvice! +julia--run-start-hook-a (inferior-buffer)
     "Run `+julia-repl-start-hook' before displaying the REPL."
     :after #'julia-repl--setup-term
     (with-current-buffer inferior-buffer
       (run-hooks '+julia-repl-start-hook)))

   (defun +julia-override-repl-escape-char-h ()
     "Use C-c instead of C-x for escaping."
     (term-set-escape-char ?\C-c)))
;;;
 (defun night/julia-repl ()
   (interactive)
   ;; (evil-insert-state 1)
   (julia-repl)
   (evil-insert-state 1))
 (after! julia-repl
   (map! :map julia-mode-map
         :localleader
         :nvi "s" #'night/julia-repl
         :nvi "ee" #'julia-repl-send-region-or-line
         ;; :nvi "l" #'julia-repl-send-region-or-line
         :nvi "el" #'julia-repl-send-line
         :nvi "eb" #'julia-repl-send-buffer
         )
   ))
;;;

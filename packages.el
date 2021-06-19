;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; @docs https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;;;
;; `doom-sync -u` is needed for the unpinnings to take effect
(unpin! hl-todo)
;;; @themes
(package! solarized-theme)
(package! humanoid-themes)
;;; @experimental
(package! dired-quick-sort)
;;;
(package! magit-section) ;; needed by doom
;;;
(package! prism
  :recipe (:host github :repo "alphapapa/prism.el"))
(package! orgmdb
  :recipe (:host github :repo "isamert/orgmdb.el"))
(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))
;;;
;; this git repo was outdated, I am installing directly from emacswiki
;; (package! highlight
;;   ;; @example
;;   :recipe (:host nil :repo "https://framagit.org/steckerhalter/highlight.el" :branch "master"))
;;;
(package! ov)                           ;; needed by our rainbow-mode
(unpin! rainbow-mode)
(package! rainbow-mode
  ;; my fork uses overlays instead of text properties so that it is not erased by hl-line
  :recipe (:host github :repo "NightMachinary/rainbow-mode")
  :pin "nil")
;; (package! rainbow-mode :recipe (:local-repo "path/to/repo"))
;;;
(package! org-super-links
  :recipe (:repo "toshism/org-super-links" :type git :host github :branch "develop"))

(package! targets
  :recipe (:repo "noctuid/targets.el" :type git :host github))
(package! exato)

(package! applescript-mode)
(package! magit-vcsh)
(package! smeargle)                     ;; Highlighting Regions by Last Updated Time
(package! org-web-tools)
(package! helm-org-rifle)
(package! zoom)
(package! sudoku)
(package! blacken)
(package! elpy)
(package! pyvenv)
(package! lispyville)
;; (package! dimmer)
(package! google-translate)
(package! move-text)
(package! rg)
(package! deadgrep)
(package! git-link)
(package! neuron-mode)
(package! nov)
(package! hungry-delete)
(package! ob-tmux)

(unpin! julia-mode julia-repl)
(package! julia-mode)
(package! julia-repl)
(package! eglot)
(package! eglot-jl)
;; (when (package! eglot)
;;   (package! eglot-jl))
;; (package! fzf)
(package! org-drill)
(package! org-ql)
(package! org-books)
(package! helm-org-ql)
(package! toc-org)
(package! org-fragtog)
(package! company-try-hard)
(package! tramp-theme)
(package! vlf)
(package! bug-hunter)
(package! telega)
(package! disk-usage)
(package! dired+)
(package! poporg)

(package! memoize)

(package! xterm-color)

(package! company-quickhelp) ;; uses pos-tip which only works in GUI
(package! company-quickhelp-terminal)

(package! company-box)

(package! eldoc-box)                    ;; GUI-only: https://github.com/casouri/eldoc-box/issues/35

;; (package! evil-better-visual-line) ; @todo fork this and reverse its behavior? Just disable evil-respect-visual...?

;;;
;; (package! gerbil-mode
;;   :recipe (:host github :repo "vyzo/gerbil"
;;            :files ("etc/gerbil-mode.el")))

;; (package! gambit-mode
;;   :recipe (:host github :repo "gambit/gambit"
;;            :files ("misc/gambit.el")))
;;;
(disable-packages! anaconda-mode)
;;;

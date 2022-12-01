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

(unpin! jupyter)

(package! racket-mode :pin "22e319754dcf650e282b3ba33b9d0ee3cda81007")
;; (package! ob-racket
;;   ;; doom uses https://github.com/DEADB17/ob-racket
;;   ;;;
;;   :recipe (:host github :repo "togakangaroo/ob-racket") ;; @bug `Invalid function: (requires (assoc 'requires params))`
;;   )
;;; @themes
(package! solarized-theme)
(package! humanoid-themes)
(package! kaolin-themes)
(package! spacemacs-theme)
(package! apropospriate-theme)
(package! darktooth-theme)
(package! rebecca-theme)
(package! solo-jazz-theme)
(package! github-theme)
(package! night-owl-theme)
(package! colorless-themes)
(package! leuven-theme)

;; [[id:c0713162-d1bd-46fc-9ef4-f5495d7ff16f][doom/bugs, issues:@upstreamBug hlissner/doom-emacs#5629 {BUG} Some themes fail to build]]
;; (package! soft-stone-theme
;;   :recipe (:build (:not native-compile compile)))
;; (package! jetbrains-darcula-theme)
;; (package! doom-themes
;;   :recipe (:build (:not native-compile compile)))

(package! base16-theme)
(package! anti-zenburn-theme)
(package! moe-theme)                    ;; Optimized for terminal’s 256 color palettes
(package! poet-theme)                   ;; Exclusively aimed at graphical emacs
(package! zaiste-theme
  :recipe (:host github :repo "zaiste/zaiste-emacs-theme"))

(package! tramp-theme)
(package! load-theme-buffer-local :recipe (:build (:not native-compile compile)))      ;; @noNativeComp
;;; @experimental
(package! dired-quick-sort)
;;;
(package! magit-section) ;; needed by doom
;;;
(package! prism
  :recipe (:host github :repo "alphapapa/prism.el"))

(package! orgmdb
  :recipe (:host github :repo "isamert/orgmdb.el"))
(package! ox-ipynb
  :recipe (:host github :repo "jkitchin/ox-ipynb"))

(package! explain-pause-mode
  :recipe (:host github :repo "lastquestion/explain-pause-mode"))

(package! teco-screenshot
  ;; [[https://github.com/tecosaur/screenshot/issues/12][tecosaur/screenshot#12 `(require 'posframe)` makes the package unusable on the TUI]]
  :recipe (:host github :repo "tecosaur/screenshot"))
;;;
(package! plz
  :recipe (:host github :repo "alphapapa/plz.el"))
(package! ement
  :recipe (:host github :repo "alphapapa/ement.el"))
;;;
(package! gnus)
;;;
(package! ediprolog)
;;;
(unpin! company-mode)
(package! helm-company)
(unpin! helm-company)
;;;
;; (package! fuz
;;   :recipe (:host github :repo "rustify-emacs/fuz.el"))

;; (package! snails
;;   :recipe (:build (:not compile) :host github :repo "manateelazycat/snails"))
;;;
(package! vundo ;; needs at least emacs@28
  :recipe (:host github :repo "casouri/vundo"))

;;;
;; this git repo was outdated, I am installing directly from emacswiki
;; (package! highlight
;;   ;; @example
;;   :recipe (:host nil :repo "https://framagit.org/steckerhalter/highlight.el" :branch "master"))
;;;
(package! ov
  :recipe (:host github :repo "emacsorphanage/ov")) ;; needed by our rainbow-mode
(unpin! rainbow-mode)
(package! rainbow-mode
  ;; my fork uses overlays instead of text properties so that it is not erased by hl-line
  :recipe (:host github :repo "NightMachinary/rainbow-mode"))
;; (package! rainbow-mode :recipe (:local-repo "path/to/repo"))
;;;
(package! org-super-links
  :recipe (:repo "toshism/org-super-links" :type git :host github :branch "develop"))

(package! targets
  :recipe (:repo "noctuid/targets.el" :type git :host github))
(package! exato)

;; (package! term-cursor
;;   :recipe (:repo "h0d/term-cursor.el" :type git :host github))
(package! evil-terminal-cursor-changer)

(package! mips-mode)

(package! async-await)

(package! org-sticky-header)

(package! applescript-mode)
(package! magit-vcsh)
(package! smeargle)                     ;; Highlighting Regions by Last Updated Time
(package! org-web-tools)
(package! helm-org-rifle)
(package! zoom)
(package! embark)
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
(package! iscroll)

(package! evil-tutor)

(package! orderless)
(package! consult)

(unpin! julia-mode julia-repl eglot-jl)
(package! julia-mode)
(package! julia-repl)
(package! eglot)
(package! eglot-jl)
;; (when (package! eglot)
;;   (package! eglot-jl))

;; (package! fzf)
(package! org-drill)
(package! org-ql)

(package! org-ref)

(package! org-vcard)

(package! org-books)
(package! helm-org-ql)
(package! toc-org)
(package! org-fragtog)

(package! nndiscourse)

(package! org-reveal)
(package! org-tree-slide :pin "d6529bc2df727d09014e0e56abf4f15a8e8fc20f")


(package! company-try-hard)

(package! a)

(package! vlf)
(package! bug-hunter)
(package! telega)
(package! disk-usage)
(package! dired+)
(package! poporg)

(package! memoize)

(package! vimrc-mode)

(package! eredis)

(package! evil-mc)

(package! outshine)
(package! navi-mode)

(package! xterm-color)

(package! code-cells)

(package! company-quickhelp) ;; uses pos-tip which only works in GUI
(package! company-quickhelp-terminal)

(unpin! company-box)
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

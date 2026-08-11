;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;;; this needs to be first, otherwise any error in our config will reset recentf
(require 'recentf)
(after! recentf
;;;
  ;; (customize-set-variable 'recentf-auto-cleanup 3600)
  ;; Cleanup each time Emacs has been idle that number of seconds.

  (customize-set-variable 'recentf-auto-cleanup 'never)
  ;; We do the cleanup ourselves.
;;;

  (setq recentf-max-saved-items 50000)
  (recentf-load-list)

  ;; @seeAlso [[./autoload/night-recentf.el]]
  )
;;;
(defmacro mycomment (&rest a)
  t)
(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)
;;;
(defun night/nop (&rest dummy))
;;;
(message "TERM: %s" (getenv "TERM"))
;;;
(require 'cl-extra)
(require 's)
(require 'f)
(require 'server)

(defun night/getenv-nonempty (name)
  "Value of environment variable NAME, or nil when unset *or empty*.
`getenv' returns \"\" for an exported-but-empty variable, which is non-nil and
therefore silently wins an `or' chain."
  (let ((v (getenv name)))
    (and v (not (string-empty-p v)) v)))

;; Defined before `night/emacs-socket-dir', which calls it while this file
;; loads; defining it later gave `void-function night/cis-p' at startup.
(defvar night/cis-p--cache 'unset
  "Memo for `night/cis-p'; it is consulted at startup, so do not re-probe.")

(defun night/cis-p ()
  "Non-nil on the LMU CIS cluster (beta, rho*, zeta*, epsilon*, ...).

The test reads ~/.night-site, whose first non-comment line names the site
profile this home belongs to; we are on CIS when that name is cis-lmu.  The
file is written by setup/bootstrap-sudoless, and since the home is the
cluster shared mount, one file covers every host in it.

The file is named for the general case rather than for CIS specifically: the
bootstrap treats a site as a variable, of which CIS is one value.

This is deliberately an *explicit* declaration rather than an inference.
Everything inferrable was tried and each was wrong in a different way:

- `file-directory-p' on the share only asks whether it happens to be
  *mounted*; any machine that mounts it would match.
- $HOME lying under a given path is a naming convention, not an identity.
  Nothing stops another site using the same one.
- The DNS search domain in /etc/resolv.conf is worse than it looks: it
  reflects network *connectivity*, not identity, so the laptop matched
  whenever it was on the LMU network.
- Hostnames would need a `hostname -f' subprocess, since `system-name' is the
  short label rather than the FQDN -- and would need updating per machine.

The result is memoised, so the cost is a single stat per Emacs session; that
makes any argument about avoiding the filesystem here irrelevant."
  (interactive)
  (if (not (eq night/cis-p--cache 'unset))
      night/cis-p--cache
    (setq night/cis-p--cache
          (let ((f (expand-file-name "~/.night-site")))
            (and (file-readable-p f)
                 (with-temp-buffer
                   (insert-file-contents f)
                   (goto-char (point-min))
                   ;; first line that is neither blank nor a comment
                   (let (name)
                     (while (and (not name) (not (eobp)))
                       (let ((l (string-trim (buffer-substring
                                              (line-beginning-position)
                                              (line-end-position)))))
                         (unless (or (string-empty-p l) (string-prefix-p "#" l))
                           (setq name l)))
                       (forward-line 1))
                     (equal name "cis-lmu"))))))))

(defun night/emacs-socket-dir ()
  "Directory for the Emacs server socket.

`~/tmp' is fine when $HOME is local, but on hosts whose home is a shared
network filesystem it is actively wrong, for two reasons:

1. A unix domain socket is a *kernel-local* IPC endpoint.  The file on the
   share is only a rendezvous name; the socket itself lives in the kernel of
   the host that bound it.  A client on another host connecting to it gets
   ECONNREFUSED -- AF_UNIX has no network transport.  So a shared socket dir
   cannot give you one Emacs across hosts, by design.
2. Worse, it collides.  `server-name' defaults to \"server\", so every host
   would use the identical path, and `server-start' deletes what it judges to
   be a stale socket -- meaning starting Emacs on one host clobbers another's.

So on such hosts, put the socket on host-local storage.  Which host-local
directory is not obvious, and getting it wrong is worse than it looks:

- $XDG_RUNTIME_DIR (/run/user/UID) is the semantically correct place, but it
  is a tmpfs that systemd-logind *deletes when your last session ends*, not
  merely at reboot.  Ubuntu leaves KillUserProcesses=no, so a daemon started
  with nohup keeps running after logout while its socket directory is removed
  underneath it -- an orphaned, unreachable Emacs still holding its memory.
  That only stops being true if lingering is enabled for the user
  \(`loginctl enable-linger'), which keeps /run/user/UID alive.
- /var/tmp survives both logout and reboot on these hosts (their tmpfiles
  config reaps only systemd-private dirs; /tmp, by contrast, is wiped at
  boot).  A socket file left over from a previous boot is harmless:
  `server-start' detects a stale socket and replaces it.

So: use $XDG_RUNTIME_DIR only when lingering is actually enabled, and
/var/tmp otherwise.  Lingering is detected by testing for the marker file
systemd creates, which is a plain `file-exists-p' -- no subprocess, so this
costs nothing at startup.

The hostname is folded into the directory name as belt-and-braces, so even if
this is somehow pointed back at a share, two hosts still cannot collide.

$NIGHT_EMACS_SOCKET_DIR overrides everything.

@warn Use `night/getenv-nonempty', not `getenv': an exported-but-empty
variable returns \"\", which is non-nil, so a plain `or' would accept it as an
override and hand `server-socket-dir' an empty path."
  (or (night/getenv-nonempty "NIGHT_EMACS_SOCKET_DIR")
      ;; Ask the declared site, not the filesystem: see `night/cis-p'.  A
      ;; mounted share proves nothing about which cluster this is.
      (when (night/cis-p)
        (let* ((user (user-login-name))
               (lingering (file-exists-p (concat "/var/lib/systemd/linger/" user)))
               (runtime (night/getenv-nonempty "XDG_RUNTIME_DIR"))
               (base (if (and lingering runtime (file-directory-p runtime))
                         runtime
                       (format "/var/tmp/%s" user))))
          (expand-file-name
           (format "emacs-servers-%s" (or (system-name) "unknown"))
           base)))
      (concat (getenv "HOME") "/tmp/.emacs-servers")))

(setq server-socket-dir (night/emacs-socket-dir))
;; This directory is created by [help:server-ensure-safe-dir] automatically.
;; see also `server-name`

(defun night/server-name-set-auto ()
  (interactive)
  (setq server-name (let ((sn (getenv "emacs_night_server_name")))
                      (cond
                       ((and sn (not (cl-equalp sn "")))
                        sn)
                       (t "server"))))
  server-name
;;;
  ;; (or
  ;;                         (and (boundp 'server-name) server-name (not (cl-equalp server-name "")) server-name)
  ;;                         (getenv "EMACS_SERVER_NAME")
  ;;                         (getenv "EMACS_SERVER_NAME"))
  )

(night/server-name-set-auto)
(defun night/server-alt1-p ()
  (interactive)
  (and (boundp 'server-name)
       (s-ends-with-p "_alt1" server-name)))

(defun night/darwin-p ()
  (interactive)
  (cl-equalp (symbol-name system-type) "darwin"))

(defun night/local-p ()
  (interactive)
  (or
   (night/darwin-p)))

(defun night/remote-p ()
  (interactive)
  (not (night/local-p)))


(defun night/pino-p ()
  (interactive)
  (cl-equalp (system-name) "Pinocchio"))

(defun night/lilf-p ()
  (interactive)
  (cl-equalp (system-name) "lilf.ir"))

(defun night/t31-p ()
  (interactive)
  (cl-equalp (system-name) "t31-gpu13"))

(defun night/t21-p ()
  (interactive)
  (cl-equalp (system-name) "t21-gpu6"))

(defun night/m17-p ()
  (interactive)
  (cl-equalp (system-name) "m17"))

(defun night/m15-p ()
  (interactive)
  (cl-equalp (system-name) "m15"))

(defun night/c0-p ()
  (interactive)
  (cl-equalp (system-name) "Taher"))

(defun night/system-name ()
  (cond
   ;; ((night/local-p) "Local")
   ((night/pino-p) "Pino")
   ((night/t31-p) "T31")
   ((night/t21-p) "T21")
   (t (system-name))))

(defvar night/server-occupied-policy 'no-server
  "What to do when another process still serves `server-name' after waiting.
- `no-server': do not start a server at all (default).
- `alt-name': append a numeric suffix (e.g., \"_1\") to `server-name' and
  start the server under the first free such name.")

(defvar night/server-occupied-wait-seconds 10
  "How long to wait for an occupied server socket to be released.")

(defun night/h-server-start ()
  (cond
   ((night/server-alt1-p)
    ;; (night/unadvice #'ivy--switch-buffer-action)
    (advice-add #'ivy--switch-buffer-action :after #'night/irc-maybe-show-count-ni))
   ;; @note +irc--ivy-switch-to-buffer-action will also call ivy--switch-buffer-action
   )
  (server-start))

(defun night/server-start-carefully ()
  "Start the Emacs server unless this Emacs already serves `server-name'.

If another process is listening on our socket, wait up to
`night/server-occupied-wait-seconds' for it to be released; if it is still
occupied afterwards, warn and follow `night/server-occupied-policy'.

See ./docs/bugs/gui-emacs-server-socket-mismatch.md for the bug this
guards against."
  (interactive)
  (let ((expected (expand-file-name server-name server-socket-dir)))
    (cond
     ;; We already own the correct socket; nothing to do. `process-live-p'
     ;; alone is not enough: an early `server-start' (e.g., from Doom's lazy
     ;; `use-package! server' in doom-editor.el) can leave `server-process'
     ;; live but bound to a default path instead of `expected'. The
     ;; `file-exists-p' check catches another process having bound and later
     ;; unlinked our socket path (e.g., `emacsclient -a ""' auto-spawning a
     ;; daemon on it): our listener would survive but be unreachable.
     ((and (process-live-p server-process)
           (equal (process-contact server-process :service) expected)
           (file-exists-p expected))
      t)
     (t
      (let ((waited 0))
        (while (and (server-running-p server-name)
                    (< waited night/server-occupied-wait-seconds))
          (message "night/server: waiting for %s to be released (%ds/%ds) ..."
                   expected waited night/server-occupied-wait-seconds)
          (sleep-for 1)
          (setq waited (1+ waited))))
      (cond
       ((not (server-running-p server-name))
        (night/h-server-start))
       (t
        (display-warning
         'night-server
         (format "could not start the server: %s is still served by another process"
                 expected))
        (cond
         ((eq night/server-occupied-policy 'alt-name)
          (let ((n 1))
            (while (server-running-p (format "%s_%d" server-name n))
              (setq n (1+ n)))
            (setq server-name (format "%s_%d" server-name n))
            (night/h-server-start)
            (display-warning
             'night-server
             (format "started the server under the alternate name %s instead"
                     server-name))))
         (t
          ;; `no-server' (default): leave this Emacs without a server.
          nil))))))))

(night/server-start-carefully)

(defun night/h-server-reassert-on-focus (&rest _)
  "Rebind the server socket if its file vanished from under us.

A stray daemon (e.g., auto-spawned by `emacsclient -a \"\"') can bind our
socket path and unlink it again when it dies, leaving our live server
unreachable. This runs on focus changes, is cheap (a `file-exists-p'),
and simply rebinds. If another process still occupies the path,
`server-start' itself refuses and warns instead of stealing."
  (when (and (frame-focus-state)
             (process-live-p server-process)
             (not (file-exists-p (expand-file-name server-name server-socket-dir))))
    (message "night/server: socket file for %s vanished; re-asserting"
             server-name)
    (server-start)))

(when (display-graphic-p)
  (add-function :after after-focus-change-function
                #'night/h-server-reassert-on-focus))

;;;
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Fereidoon"
      user-mail-address "rudiwillalwaysloveyou@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 29))
(setq doom-font (font-spec :family "Fira Mono" :size 29))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(progn
  (require 'doom-themes-ext-org)        ;; @workaround for upstream bugs

  (require 'solarized-theme)
  (require 'humanoid-themes)
  (require 'kaolin-themes)
  ;; (require 'spacemacs-theme)
  (require 'apropospriate-theme)
  (require 'darktooth-theme)
  (require 'rebecca-theme)
  (require 'solo-jazz-theme)
  (require 'github-theme)
  (require 'night-owl-theme)
  (require 'colorless-themes)
  (require 'leuven-theme)
  (require 'base16-theme)
  (require 'anti-zenburn-theme)
  (require 'moe-theme)
  (require 'poet-theme)
  (require 'zaiste-theme)
  (require 'modus-themes)
  (require 'ef-themes)
  (require 'material-theme)
  (require 'tao-theme)
  (require 'organic-green-theme)
  (require 'color-theme-sanityinc-tomorrow))
(cond
 ((night/lilf-p)
  (setq night/current-theme-light 'ef-frost
        night/current-theme-dark 'ef-winter))
 ((night/pino-p)
  (setq night/current-theme-light 'modus-operandi-deuteranopia
        night/current-theme-dark 'modus-vivendi-deuteranopia))
 ((night/cis-p)
  (setq night/current-theme-light 'modus-operandi-deuteranopia
        night/current-theme-dark 'modus-vivendi-deuteranopia))
 ((night/t31-p)
  (setq night/current-theme-light 'modus-operandi-tinted
        night/current-theme-dark 'modus-vivendi-tinted))
 ((night/t21-p)
  (setq night/current-theme-light 'modus-operandi-tinted
        night/current-theme-dark 'modus-vivendi-tinted))
 ((night/m15-p)
  (setq night/current-theme-light 'sanityinc-tomorrow-day
        night/current-theme-dark 'sanityinc-tomorrow-night))
 ((night/m17-p)
  (setq night/current-theme-light 'ef-day
        night/current-theme-dark 'ef-night))
 ((night/c0-p)
  (setq night/current-theme-light 'ef-cyprus
        night/current-theme-dark 'ef-dark))
 (;; nil
  (night/server-alt1-p)
  ;; (setq night/current-theme-light 'doom-one-light)
  ;; [[id:c0713162-d1bd-46fc-9ef4-f5495d7ff16f][doom/bugs, issues:@upstreamBug hlissner/doom-emacs#5629 {BUG} Some themes fail to build]]


  (setq night/current-theme-light 'solarized-selenized-white
        night/current-theme-dark 'solarized-selenized-dark)
  ;; (setq night/current-theme-light 'tsdh-light)
  ;; (setq night/current-theme-light 'solarized-light)
  )
 ((display-graphic-p)
  (setq night/current-theme-light 'modus-operandi-tritanopia
        night/current-theme-dark 'modus-vivendi-tritanopia
        ;; 'kaolin-light
        ;; 'solarized-selenized-white
        ;; 'doom-one-light
        ;; 'humanoid-light
        ;; 'doom-nord-light (has some bad color choices for orgmode)
        ;; 'doom-ayu-light
        ;; 'doom-one-light
        ))
 (t
  (setq night/current-theme-light 'modus-operandi-tritanopia
        night/current-theme-dark 'modus-vivendi-tritanopia
        ;; 'kaolin-light
        ;; 'solarized-selenized-white
        ;; 'doom-one-light
        ;; 'humanoid-light
        ;; 'doom-nord-light (has some bad color choices for orgmode)
        ;; 'doom-ayu-light
        ;; 'doom-one-light
        )
  ;; (setq night/current-theme-light 'solarized-light)
  ;; (setq night/current-theme-light 'solarized-selenized-light) ;; @good
  ;; (setq night/current-theme-light 'doom-solarized-light) ; subtly different
  ))
(defun night/terminal-dark-p ()
  "Best available answer to: is the terminal background dark?

Emacs cannot ask a terminal for its background colour.  The one convention is
the COLORFGBG environment variable (fg;bg colour indices, from rxvt); kitty and
iTerm2 do not set it, and with it absent Emacs guesses -- badly, it assumes
dark.  Sources are consulted in order of how much they actually know:

1. NIGHT_EMACS_THEME, an explicit override (values: dark, light).
2. COLORFGBG, if the terminal or the shell provides it.  Over ssh it has to be
   smuggled: sshd here accepts only LANG and LC_*, so kitty sets
   LC_COLORFGBG and the env contract copies it into COLORFGBG server-side.
   See setup/bootstrap-sudoless/README.org.
3. Otherwise assume light, which is what this terminal actually is; the old
   implicit answer was dark, and that is what made faces look washed out."
  (let ((explicit (night/getenv-nonempty "NIGHT_EMACS_THEME"))
        (cfb (night/getenv-nonempty "COLORFGBG")))
    (cond
     (explicit (equal explicit "dark"))
     (cfb
      ;; The background index is the last field.  Emacs treats 0-6 and 8 as
      ;; dark in `frame-set-background-mode'; mirror that.
      (and (member (car (last (split-string cfb ";")))
                   (list "0" "1" "2" "3" "4" "5" "6" "8"))
           t))
     (t nil))))

(defvar night/theme-dark-p (night/terminal-dark-p)
  "Whether to use the dark variant.  See `night/terminal-dark-p'.")

(setq doom-theme
      (if night/theme-dark-p night/current-theme-dark night/current-theme-light))

;; @warn Emacs cannot ask a terminal for its background colour.  The only
;; signal is $COLORFGBG, which kitty and iTerm2 do not set; with it unset Emacs
;; *guesses*, and it guesses dark.  So on a light terminal every face is
;; computed for the wrong polarity and the result looks washed out and
;; low-contrast -- regardless of which theme was selected.  That, not the theme
;; choice, is what made the CIS servers look broken.
;;
;; So state the polarity explicitly, keeping it consistent with the variant we
;; just chose.  `frame-background-mode' is the documented override and applies
;; to terminal and GUI frames alike; existing frames need
;; `frame-set-background-mode' to recompute.
(setq frame-background-mode (if night/theme-dark-p 'dark 'light))
(mapc #'frame-set-background-mode (frame-list))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory (concat (getenv "cellar") "/notes/org"))

(setq night/roam-p nil)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; #spacemacs config imported
(setq evil-respect-visual-line-mode nil)
(load! "night-loader.el")

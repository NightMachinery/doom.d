;;; autoload/night-savehist.el -*- lexical-binding: t; -*-
;;;
(after! savehist
  (savehist-mode)

  ;; [[https://discord.com/channels/406534637242810369/1107093795662729298][(518) Discord | "Where should I set `savehist-autosave-interval`?" | Doom Emacs]]
  (setq savehist-autosave-interval 300)
  ;; Doom (?) sets to nil; it will only use hooks then, I assume.
  ;; [help:savehist-install]
  (savehist-install) ;; makes the above var take effect (installs the timer)

  (add-to-list 'savehist-additional-variables 'jupyter-server-kernel-names))
;;;

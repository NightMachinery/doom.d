;;; autoload/scheme/night-gerbil.el -*- lexical-binding: t; -*-

(comment
 (use-package gambit-mode
   :defer t
   :config
   (add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode))

 (use-package gerbil-mode
   :defer t
   :config
   (defvar gerbil-program-name
     ;; depending on your env setup, this can probably be just "gxi"
     (expand-file-name "/usr/local/bin/gxi"))
   (setq scheme-program-name gerbil-program-name))

;;;
 ;; (visit-tags-table "~/.gerbil/pkg/TAGS")
 ;; The Gerbil package manager also generates a TAGS table for all installed packages; by default this lives in ~/.gerbil/pkg/TAGS
 ;; `bi gumbo-parser ; gxpkg install github.com/danielsz/gerbil-gumbo` to install a package
 ;; did not generate the TAGS file for me though :|
;;;

 (visit-tags-table "/usr/local/Cellar/gerbil-scheme/0.16/share/gerbil/TAGS")
 ;; @fragile This path needs to be set @manually for now
4                                       ;49;1M
 (after! gerbil-mode
   (map! :map gerbil-mode-map
         :localleader :desc "Run Scheme REPL"
         "'" (cmd! (pop-to-buffer "*scheme*")
                   (run-scheme scheme-program-name))
         :desc "Send region"
         "," #'scheme-send-region
         :desc "Send definition"
         "e" #'scheme-send-definition
         :desc "Send definition and go"
         "E" #'scheme-send-definition-and-go
         :desc "Load a file"
         "l" #'scheme-load-file))
 )

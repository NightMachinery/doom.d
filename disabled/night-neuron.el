;; forked from https://gist.github.com/felko/cdb3fc19b3a60db27eb3c5bd319fc479

(defface neuron-stub-face
  '((((class color) (min-colors 88) (background dark)) :foreground "#C16069" :underline "#C16069")
    (((class color) (min-colors 88) (background light)) :foreground "#C16069" :underline "#C16069")
    (((class color) :foreground "Red" :underline "Red"))
    (t :inherit neuron-title-overlay-face))
  "Face for stub links."
  :group 'neuron-faces)

(customize-set-variable 'neuron-default-zettelkasten-directory (expand-file-name (getenv "ZETTLE_DIR")))

(customize-set-variable 'neuron-default-tags (list "stub"))

(customize-set-variable 'neuron-tag-specific-title-faces '(("stub" neuron-stub-face)))

(defun search-zettelkasten ()
  "Search zettels by content."
  (interactive)
  (progn
    (+ivy-file-search :in (neuron-zettelkasten) :recursive nil :prompt "Search Zettelkasten: ")
    (neuron-mode)))

(defun find-file-in-zettelkasten ()
  "Find a file in the currently active zettelkasten."
  (interactive)
  (let ((default-directory (neuron-zettelkasten)))
    (counsel-find-file)))

(use-package! neuron-mode
  :config
  (map! :leader
        (:prefix ("r" . "zettel")
         "z" #'neuron-new-zettel
         "e" #'neuron-edit-zettel
         "w" #'neuron-rib-watch
         "g" #'neuron-rib-generate
         "o" #'neuron-open-zettel
         "O" #'neuron-open-index
         "j" #'neuron-open-daily-notes
         "t" #'neuron-query-tags
         "r" #'neuron-refresh
         "c" #'neuron-edit-zettelkasten-configuration

          ;;; Alternatively, bind all rib commands in a separate prefix
         ;; (:prefix ("r" . "rib")
         ;;   "w" #'neuron-rib-watch
         ;;   "g" #'neuron-rib-generate
         ;;   "s" #'neuron-rib-serve
         ;;   "o" #'neuron-rib-open-zettel
         ;;   "z" #'neuron-rib-open-z-index
         ;;   "k" #'neuron-rib-kill
         ;;   )
         )
        )

  (map! :map neuron-mode-map
        :localleader
        ;; Override markdown-mode's default behavior to handle neuron links
        :ni "o" #'neuron-follow-thing-at-point

        ;; You can also remove the "z" prefix but
        ;; be careful not to override default
        ;; markdown-mode bindings.
        ;; (:prefix ("r" . "zettel")
        :ni "u" #'neuron-edit-uplink
        :ni "t" #'neuron-add-tag
        :ni "T" #'neuron-add-tags
        :ni "O" #'neuron-open-current-zettel
        :ni "l" #'neuron-create-and-insert-zettel-link
        :v  "L" #'neuron-create-zettel-from-selected-title ;; #'neuron-create-zettel-from-selection
        :ni "s" #'neuron-insert-static-link
        :ni "c" #'neuron-toggle-connection-type
        ;; )
        )

  (map! :leader "sz" #'search-zettelkasten)
  (map! :leader "fz" #'find-file-in-zettelkasten)
  )

(defun night/neuron-activator ()
  "Alt: neuron--auto-enable-when-in-zettelkasten (uses neuron.dhall, we use ZETTLE_DIR)"
  (interactive)
  (let  ((dir (file-name-directory buffer-file-name)))
    (print "hi")
    (print (getenv "ZETTLE_DIR"))
    (print dir)
    (when (and (eq major-mode 'markdown-mode) (string-prefix-p (getenv "ZETTLE_DIR") dir))
      (neuron-mode)
      )
    )
  )

;; (add-hook 'markdown-mode-hook #'night/neuron-activator)

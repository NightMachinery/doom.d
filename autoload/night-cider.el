;;; ~/doom.d/night-cider.el -*- lexical-binding: t; -*-

(progn
  (add-hook 'clojure-mode-hook  #'my-lisp-init)

  (after! (cider lispy le-clojure)

    (progn
      (add-hook 'cider--debug-mode-hook (lambda ()
                                          (if (bound-and-true-p cider--debug-mode)
                                              (turn-off-evil-snipe-mode)
                                            (turn-on-evil-snipe-mode))))
      (add-hook 'cider-connections-buffer-mode-hook (lambda ()
                                                      (evil-insert-state 1)))

      (defun my-cider-eval-paragraph ()
        (interactive)
        (save-excursion
          (mark-paragraph)
          (command-execute 'cider-eval-region)))

      (defun night/cider-eval-line ()
        "Evals the current line, skipping over unmatched delimiters"
        (interactive)
;;;
        (cl-block nil
          (dolist (r (lispy--find-safe-regions
                      (line-beginning-position)
                      (line-end-position)))
            (let* (
                   (beg (car r))
                   (end (cdr r))
                   (res (cider-eval-region beg end)))
            ;;;
              ;; trying to abort evaling the rest of sexps when an error happens.
              ;; I could not find a way to detect errors from elisp.
              (comment
               (message "night/cider-eval-line: res: %s" res)
               (if (not res)
                   (progn
                     (message "night/cider-eval-line: eval failed")
                     (cl-return)))))))
       ;;; old implementation that did not skip unmatched delimiters
        ;; (save-mark-and-excursion
        ;;   (my-select-current-line)
        ;;   (command-execute 'cider-eval-region))
        )

      (evil-define-key 'normal clojure-mode-map (kbd "\\ e l") #'night/cider-eval-line)
;;;
      ;; see:
      ;; - [help:lispy-cider-jack-in-dependencies]
      ;; - https://github.com/abo-abo/lispy/issues/609
      ;;
      ;; if you use lispy's =e= to start the CIDER server, it will inject the necessary stuff automtically
      ;;
      ;; beware that this can cause a lower version of nREPL to be used
      (setq cider-jack-in-dependencies
            (delete-dups
             (append
              cider-jack-in-dependencies
              lispy-cider-jack-in-dependencies))))
;;;
    ))

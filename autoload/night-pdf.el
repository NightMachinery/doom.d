;;; ~/doom.d/autoload/night-pdf.el -*- lexical-binding: t; -*-
;; [[nightNotes:subjects/software/emacs/pdf.org][emacs/pdf]]
;;;
(require 'image-roll)
(require 'pdf-tools)
(require 'pdf-view)
(require 'pdf-history)
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
;; (add-to-list 'auto-mode-alist '("\\.pdf\\'" . doc-view-mode))

(setq pdf-tools-enabled-modes
      (-remove-item 'pdf-annot-minor-mode pdf-tools-enabled-modes))

(setq pdf-view-resize-factor 1.05)

(defun night/pdf-hook-fn ()
  (interactive)
  (night/disable-line-numbers)
  (pdf-view-fit-width-to-window) ;; you can also just press W
  (night/horizontal-scroll-enable-locally)
  (pdf-outline-imenu-enable)

  (comment
   (when (fboundp 'pdf-continuous-scroll-mode)
     (night/pdf-continuous-startup)))

  (progn
    (when (fboundp 'pdf-view-roll-minor-mode)
      (night/pdf-view-roll-startup))))

(defun night/pdf-view-roll-startup ()
  (pdf-view-roll-minor-mode))

(defun night/pdf-view-scroll-previous ()
  (interactive)
  (dotimes (_ 4)
    (pdf-view-previous-line-or-previous-page 1)))
(defun night/pdf-view-scroll-next ()
  (interactive)
  (dotimes (_ 4)
    (pdf-view-next-line-or-next-page 1)))

(map!
 :map pdf-view-roll-minor-mode-map
 "<pdf-view-text-region> <wheel-up>" #'night/pdf-view-scroll-previous
 "<wheel-up>" #'night/pdf-view-scroll-previous
 :gnvi
 "S-<up>" #'night/pdf-view-scroll-previous

 "<pdf-view-text-region> <wheel-down>" #'night/pdf-view-scroll-next
 "<wheel-down>" #'night/pdf-view-scroll-next
 :gnvi
 "S-<down>" #'night/pdf-view-scroll-next

 "S-<left>" #'night/scroll-right
 "S-<right>" #'night/scroll-left)

(defun night/pdf-continuous-startup ()
  (pdf-cscroll-toggle-mode-line)
  (pdf-continuous-scroll-mode)
  (define-key pdf-continuous-scroll-mode-map
              (kbd "<wheel-up>")
              #'(lambda ()
                  (interactive)
                  (dotimes (_ 4) (pdf-continuous-scroll-backward-line 1))))
  (define-key
   pdf-continuous-scroll-mode-map
   (kbd "<wheel-down>")
   #'(lambda ()
       (interactive)
       (dotimes (_ 4) (pdf-continuous-scroll-forward-line 1))))
;;;
  (after! evil-collection
    (progn
      (defun evil-collection-pdf-view-next-line-or-next-page (&optional count)
        "'evil' wrapper include a count argument to `pdf-view-next-line-or-next-page'"
        (interactive "P")
        (if count
            ;; (dotimes (_ count nil)
            ;; (pdf-view-next-line-or-next-page 1))
            (pdf-view-next-line-or-next-page count)

          ;; (pdf-continuous-scroll-forward)
          (dotimes (_ night/pdf-scroll-step) (pdf-continuous-scroll-forward-line 1))
          ;; (pdf-continuous-scroll-forward night/pdf-scroll-step)
          ;; (pdf-view-next-line-or-next-page night/pdf-scroll-step)
          ))
      (defun evil-collection-pdf-view-previous-line-or-previous-page (&optional count)
        "'evil' wrapper include a count argument to `pdf-view-previous-line-or-previous-page'"
        (interactive "P")
        (if count
            ;; (dotimes (_ count nil)
            ;; (pdf-view-previous-line-or-previous-page 1))
            (pdf-view-previous-line-or-previous-page count)

          ;; (pdf-continuous-scroll-backward)
          ;; (pdf-continuous-scroll-backward night/pdf-scroll-step)
          (dotimes (_ night/pdf-scroll-step) (pdf-continuous-scroll-backward-line 1))
          ;; (pdf-view-previous-line-or-previous-page night/pdf-scroll-step)
          ))
      )))

(progn
  (add-hook 'pdf-view-mode-hook #'night/pdf-hook-fn)

  (setq-default night/pdf-scroll-step 20)
  ;; (setq pdf-continuous-step-size night/pdf-scroll-step)

  (comment (evil-define-minor-mode-key 'evilified 'pdf-continuous-scroll-mode
             "j" #'pdf-continuous-scroll-forward
             (kbd "<mouse-5>") #'pdf-continuous-scroll-forward
             "k" #'pdf-continuous-scroll-backward
             (kbd "<mouse-4>") #'pdf-continuous-scroll-backward
             "J" #'pdf-continuous-next-page
             "K" #'pdf-continuous-previous-page
             (kbd "C-j") #'pdf-view-scroll-up-or-next-page
             (kbd "C-k") #'pdf-view-scroll-down-or-previous-page
             (kbd "g t") #'pdf-cscroll-view-goto-page
             (kbd "g g") #'pdf-cscroll-first-page
             "G" #'pdf-cscroll-last-page
             "M" #'pdf-cscroll-toggle-mode-line
             "Q" #'pdf-cscroll-kill-buffer-and-windows
             "l" #'pdf-cscroll-image-forward-hscroll
             "h" #'pdf-cscroll-image-backward-hscroll))
  )

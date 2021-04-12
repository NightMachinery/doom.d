;;; B
;;; ~
;;; /doom.d/autoload/night-ivy.el -*- lexical-binding: t; -*-

(with-eval-after-load 'ivy
  (push (cons #'swiper (cdr (assq t ivy-re-builders-alist)))
        ivy-re-builders-alist)
  (push (cons t #'ivy--regex-fuzzy) ivy-re-builders-alist))

(after! (ivy  counsel)
  (defun night/ivy--directory-out ()
    (let (dir)
      (when (and
             ;; instead of ivy--directory: (not as good) (ivy-state-current ivy-last)
             (setq dir (ivy-expand-file-if-directory (concat ivy--directory "/../"))))
        (ivy--cd dir)
        (ivy--exhibit))))

  (define-key counsel-find-file-map (kbd "<left>")
    (lambda ()
      (interactive)
      (night/ivy--directory-out)
      ))
  (define-key counsel-find-file-map (kbd "<right>")
    (lambda ()
      (interactive)
      (ivy--directory-enter)
      ))
  ;;;
  (defun night/ivy-mark-toggle ()
    "Mark/unmark the selected candidate."
    (interactive)
    (if (ivy--marked-p)
        (ivy--unmark (ivy-state-current ivy-last))
      (ivy--mark (ivy-state-current ivy-last)))
    )
  (defun night/ivy-mark-toggle-up ()
    (interactive)
    (night/ivy-mark-toggle)
    (ivy-previous-line))
  (defun night/ivy-mark-toggle-down ()
    (interactive)
    (night/ivy-mark-toggle)
    (ivy-next-line))
  ;; @rememberMe
  (define-key ivy-minibuffer-map (kbd "TAB") #'night/ivy-mark-toggle)
  (define-key ivy-minibuffer-map (kbd "S-<up>") 'night/ivy-mark-toggle-up)
  (define-key ivy-minibuffer-map (kbd "S-<down>") 'night/ivy-mark-toggle-down)
  ;; (define-key ivy-minibuffer-map (kbd "S-TAB") 'ivy-unmark)
  ;; (define-key ivy-minibuffer-map (kbd "<backtab>") 'ivy-unmark)
  ;;;
  )

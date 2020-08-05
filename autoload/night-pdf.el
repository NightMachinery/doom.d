;;; ~/doom.d/autoload/night-pdf.el -*- lexical-binding: t; -*-

(defun night/disable-line-numbers ()
  (interactive)
  (display-line-numbers-mode -1)
  )
(defun night/pdf-hook-fn ()
  (interactive)
  (night/disable-line-numbers)
  (pdf-view-fit-width-to-window)  ;; you can also just press W
  )
(add-hook 'pdf-view-mode-hook #'night/pdf-hook-fn)

(setq-default night/pdf-scroll-step 30)

(after! evil-collection (progn (defun evil-collection-pdf-view-next-line-or-next-page (&optional count)
                                 "'evil' wrapper include a count argument to `pdf-view-next-line-or-next-page'"
                                 (interactive "P")
                                 (if count
                                     ;; (dotimes (_ count nil)
                                     ;; (pdf-view-next-line-or-next-page 1))
                                     (pdf-view-next-line-or-next-page count)
                                   (pdf-view-next-line-or-next-page night/pdf-scroll-step)))
                               (defun evil-collection-pdf-view-previous-line-or-previous-page (&optional count)
                                 "'evil' wrapper include a count argument to `pdf-view-previous-line-or-previous-page'"
                                 (interactive "P")
                                 (if count
                                     ;; (dotimes (_ count nil)
                                     ;; (pdf-view-previous-line-or-previous-page 1))
                                     (pdf-view-previous-line-or-previous-page count)
                                   (pdf-view-previous-line-or-previous-page night/pdf-scroll-step)))
                               ))

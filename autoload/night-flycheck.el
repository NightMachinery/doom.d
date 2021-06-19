;;; autoload/night-flycheck.el -*- lexical-binding: t; -*-

(defun night/disable-flycheck ()
  (interactive)
  (flycheck-mode -1))
;;;
;; https://emacs.stackexchange.com/questions/47878/how-can-i-disable-a-specific-lint-error-for-emacs-lisp-using-flycheck
(defcustom flycheck-noflycheck-marker "noflycheck"
  "Flycheck line regions marked with this marker string are ignored."
  :type 'string
  :group 'flycheck)

(defun flycheck-noflycheck (err)
  "Ignore flycheck if line of ERR ends with `flycheck-noflycheck-marker'."
  (save-excursion
    (goto-char (cdr (flycheck-error-region-for-mode err 'symbols)))
    (looking-back flycheck-noflycheck-marker
          (max (- (point) (length flycheck-noflycheck-marker))
               (point-min)))))

(defun noflycheck-hook ()
  "Add the ;;;###noflycheck thing to elisp."
  (require 'flycheck)
  (add-hook 'flycheck-process-error-functions #'flycheck-noflycheck nil t))

(add-hook 'prog-mode-hook #'noflycheck-hook)
;;;

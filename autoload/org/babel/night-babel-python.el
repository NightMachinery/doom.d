;;; autoload/org/babel/night-babel-python.el -*- lexical-binding: t; -*-
(after! (org evil-org)
  (defun night/python-indent-shift (beg end count)
    "Indent Python code between BEG and END by COUNT spaces.
Positive COUNT shifts right, negative COUNT shifts left.
Uses `python-indent-shift-right` and `python-indent-shift-left` as appropriate."
    (when (and beg end)
      (let* ((count (or count 1))       ; Default to 1 if count is nil
             (adjusted-count (* count (or python-indent-offset 4)))
             (shift-fn (if (>= count 0)
                           #'python-indent-shift-right
                         #'python-indent-shift-left)))
        (save-excursion
          (funcall shift-fn beg end (abs adjusted-count)))))
    (when (and evil-org-retain-visual-state-on-shift (evil-visual-state-p))
    (evil-normal-state)
    (evil-visual-restore)))

  (defun night/org-in-python-src-block-p ()
    "Check if point is in a Python source block."
    (when (org-in-src-block-p)
      (let ((lang (org-element-property :language (org-element-at-point))))
        (and lang (string-match-p "^\\(jupyter-\\)?python$" (downcase lang))))))

  (defun night/h-evil-org->-py (orig-fun beg end count &rest args)
    "Advice around `evil-org->` to use Python-specific indentation in Python source blocks."
    (if (night/org-in-python-src-block-p)
        (night/python-indent-shift beg end count)
      (apply orig-fun beg end count args)))

  (advice-add 'evil-org-> :around #'night/h-evil-org->-py))

;;; autoload/night-string.el -*- lexical-binding: t; -*-

(declare-function night/insert-for-yank "night-clipboard" (text))

(defun night/escape-spaces-with-backslash (string)
  "Escape spaces in STRING with backslashes."
  (replace-regexp-in-string " " "\\\\ " string))

(defun night/h-sentence-case-letter-p (char)
  "Return non-nil when CHAR is an alphabetic character."
  (string-match-p "\\`[[:alpha:]]\\'" (char-to-string char)))

(defun night/h-sentence-case-all-uppercase-p (text)
  "Return non-nil when TEXT has letters but no lowercase letters."
  (let ((case-fold-search nil))
    (and (string-match-p "[[:alpha:]]" text)
         (not (string-match-p "[[:lower:]]" text)))))

(defun night/h-sentence-case-transform (text)
  "Return TEXT with sentence-starting words capitalized."
  (let ((normalized-text
         (cond
          ((night/h-sentence-case-all-uppercase-p text)
           (downcase text))
          (t text)))
        (capitalize-next-p t)
        (result nil))
    (mapc
     (lambda (char)
       (let ((out-char
              (cond
               ((and capitalize-next-p
                     (night/h-sentence-case-letter-p char))
                (setq capitalize-next-p nil)
                (upcase char))
               (t char))))
         (push out-char result)
         (cond
          ((memq char '(?. ?? ?! ?\n))
           (setq capitalize-next-p t)))))
     normalized-text)
    (apply #'string (nreverse result))))

;;;###autoload
(defun night/sentence-case (&optional text insert-p)
  "Sentence-case TEXT.

Capitalize sentence-starting words while leaving existing non-start
case untouched.  When TEXT has letters but no lowercase letters,
downcase it first and then sentence-case it.

Interactively, read text from the clipboard/kill-ring and insert the
result at point."
  (interactive (list (current-kill 0) t))
  (let ((result
         (night/h-sentence-case-transform
          (cond
           ((stringp text)
            text)
           (t
            (error "night/sentence-case: expected a string"))))))
    (cond
     (insert-p
      (night/insert-for-yank result))
     (t result))))

;;; autoload/night-string.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(declare-function night/insert-for-yank "night-clipboard" (text))

(defcustom night/sentence-case-enable-replacements t
  "When non-nil, `night/sentence-case' applies word replacements by default."
  :type 'boolean
  :group 'night)

(defcustom night/sentence-case-replacements
  '(("i" . "I")
    ("sth" . "something")
    ("whats" . "what's")
    ("thats" . "that's")
    ("dont" . "don't")
    ("cant" . "can't")
    ("wont" . "won't")
    ("im" . "I'm")
    ("ive" . "I've")
    ("id" . "I'd")
    ("ill" . "I'll"))
  "Ordered whole-word replacements for `night/sentence-case'."
  :type '(alist :key-type string :value-type string)
  :group 'night)

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

(defun night/h-sentence-case-capitalized-p (text)
  "Return non-nil when TEXT starts uppercase and then has no uppercase letters."
  (let ((case-fold-search nil))
    (and (string-match-p "\\`[[:upper:]]" text)
         (not (string-match-p "[[:upper:]]" (substring text 1))))))

(defun night/h-sentence-case-upcase-initial (text)
  "Return TEXT with only its first character uppercased."
  (concat (upcase (substring text 0 1))
          (substring text 1)))

(defun night/h-sentence-case-replacement-target (source target)
  "Return case-aware TARGET for matched SOURCE."
  (cond
   ((night/h-sentence-case-all-uppercase-p source)
    (upcase target))
   ((night/h-sentence-case-capitalized-p source)
    (night/h-sentence-case-upcase-initial target))
   (t target)))

(defun night/h-sentence-case-apply-replacements (text)
  "Apply `night/sentence-case-replacements' to whole words in TEXT."
  (let ((case-fold-search t)
        (result text))
    (dolist (replacement night/sentence-case-replacements result)
      (let ((source (car replacement))
            (target (cdr replacement)))
        (setq result
              (replace-regexp-in-string
               (concat "\\_<" (regexp-quote source) "\\_>")
               (lambda (match)
                 (night/h-sentence-case-replacement-target match target))
               result
               nil
               nil))))))

(defun night/h-sentence-case-transform (text replacements-p)
  "Return TEXT with sentence-starting words capitalized."
  (let* ((replaced-text
          (cond
           (replacements-p
            (night/h-sentence-case-apply-replacements text))
           (t text)))
         (normalized-text
          (cond
           ((night/h-sentence-case-all-uppercase-p replaced-text)
            (downcase replaced-text))
           (t replaced-text)))
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
(cl-defun night/sentence-case (&optional text insert-p (replacements-p nil replacements-p-supplied-p))
  "Sentence-case TEXT.

Apply optional whole-word replacements, then capitalize sentence-starting
words while leaving existing non-start case untouched.  When TEXT has
letters but no lowercase letters, downcase it first and then sentence-case it.

When REPLACEMENTS-P is omitted, use
`night/sentence-case-enable-replacements'.  When it is explicitly nil, skip
the replacement pass.

Interactively, read text from the clipboard/kill-ring and insert the
result at point."
  (interactive (list (current-kill 0) t))
  (let ((result
         (night/h-sentence-case-transform
          (cond
           ((stringp text)
            text)
           (t
            (error "night/sentence-case: expected a string")))
          (cond
           (replacements-p-supplied-p
            replacements-p)
           (t night/sentence-case-enable-replacements)))))
    (cond
     (insert-p
      (night/insert-for-yank result))
     (t result))))

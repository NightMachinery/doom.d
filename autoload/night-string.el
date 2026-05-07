;;; autoload/night-string.el -*- lexical-binding: t; -*-

(require 'cl-lib)

(declare-function night/insert-for-yank "night-clipboard" (text))

(defcustom night/sentence-case-enable-replacements t
  "When non-nil, `night/sentence-case' applies word replacements by default."
  :type 'boolean
  :group 'night)

(defcustom night/sentence-case-always-replacements
  '(("i" . "I"))
  "Ordered whole-word replacements always applied by `night/sentence-case'."
  :type '(alist :key-type string :value-type string)
  :group 'night)

(defcustom night/sentence-case-replacements
  '(("sth" . "something")
    ("smth" . "something")
    ("smt" . "something")
    ("tho" . "though")
    ("thru" . "through")
    ("pls" . "please")
    ("plz" . "please")
    ("abt" . "about")
    ("whats" . "what's")
    ("thats" . "that's")
    ("heres" . "here's")
    ("theres" . "there's")
    ("wheres" . "where's")
    ("hows" . "how's")
    ("whos" . "who's")
    ("whens" . "when's")
    ("whys" . "why's")
    ("dont" . "don't")
    ("cant" . "can't")
    ("wont" . "won't")
    ("isnt" . "isn't")
    ("arent" . "aren't")
    ("wasnt" . "wasn't")
    ("werent" . "weren't")
    ("hasnt" . "hasn't")
    ("havent" . "haven't")
    ("hadnt" . "hadn't")
    ("didnt" . "didn't")
    ("doesnt" . "doesn't")
    ("wouldnt" . "wouldn't")
    ("couldnt" . "couldn't")
    ("shouldnt" . "shouldn't")
    ("mustnt" . "mustn't")
    ("im" . "I'm")
    ("ive" . "I've")
    ("id" . "I'd")
    ("ill" . "I'll")
    ("youre" . "you're")
    ("youve" . "you've")
    ("youd" . "you'd")
    ("youll" . "you'll")
    ("theyre" . "they're")
    ("theyve" . "they've")
    ("theyd" . "they'd")
    ("theyll" . "they'll")
    ("weve" . "we've")
    ("hes" . "he's")
    ("shes" . "she's")
    ("itll" . "it'll")
    ("itd" . "it'd")
    ("imple?" . "implementation")
    ("wouldve" . "would've")
    ("couldve" . "could've")
    ("shouldve" . "should've")
    ("mustve" . "must've")
    ("mightve" . "might've")
    ("yall" . "y'all"))
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

(defun night/h-sentence-case-apply-replacements (text replacements)
  "Apply REPLACEMENTS to whole words in TEXT."
  (let ((case-fold-search t)
        (result text))
    (dolist (replacement replacements result)
      (let ((source (car replacement))
            (target (cdr replacement)))
        (setq result
              (replace-regexp-in-string
               (concat "\\_<"
                       ;; (regexp-quote source)
                       source
                       "\\_>")
               (lambda (match)
                 (night/h-sentence-case-replacement-target match target))
               result
               nil
               nil))))))

(defun night/h-sentence-case-transform (text replacements-p)
  "Return TEXT with sentence-starting words capitalized."
  (let* ((always-replaced-text
          (night/h-sentence-case-apply-replacements
           text
           night/sentence-case-always-replacements))
         (replaced-text
          (cond
           (replacements-p
            (night/h-sentence-case-apply-replacements
             always-replaced-text
             night/sentence-case-replacements))
           (t always-replaced-text)))
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
(cl-defun night/sentence-case (text &key insert-p (replacements-p night/sentence-case-enable-replacements))
  "Sentence-case TEXT.

Apply optional whole-word replacements, then capitalize sentence-starting
words while leaving existing non-start case untouched.  When TEXT has
letters but no lowercase letters, downcase it first and then sentence-case it.

When REPLACEMENTS-P is omitted, use
`night/sentence-case-enable-replacements'.  When it is explicitly nil, skip
the replacement pass.

Interactively, read text from the clipboard/kill-ring and insert the
result at point."
  (interactive (list (current-kill 0) :insert-p t))
  (let ((result
         (night/h-sentence-case-transform
          (cond
           ((stringp text)
            text)
           (t
            (error "night/sentence-case: expected a string")))
          replacements-p)))
    (cond
     (insert-p
      (night/insert-for-yank result))
     (t result))))

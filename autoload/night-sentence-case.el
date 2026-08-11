;;; autoload/night-sentence-case.el -*- lexical-binding: t; -*-

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

(defcustom night/sentence-case-abbreviations
  '("e.g." "i.e." "etc." "cf." "vs." "al." "approx." "fig." "no." "vol."
    "eq." "ref." "resp." "viz." "ca." "mr." "mrs." "ms." "dr." "prof."
    "jr." "sr." "inc." "est." "st." "ed." "eds." "pp." "ch." "sec."
    "dept." "univ." "co." "ltd." "min." "max." "avg." "incl." "excl.")
  "Lowercase abbreviations whose trailing dot does not end a sentence.

The tradeoff is deliberate.  A sentence that genuinely ends in one of
these leaves the next word uncapitalized, which is cosmetic; the
alternative is capitalizing the word after every `e.g.', which is
wrong far more often."
  :type '(repeat string)
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
    ;; ("id" . "I'd")  ;; "ID" will be corrupted if enabled.
    ("idk" . "I don't know")
    ("ill" . "I'll")
    ("iirc" . "if I recall correctly")
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

(defconst night/h-sentence-case-opening-regexp
  "[\"'([{*_‘“«]"
  "Punctuation skipped when looking for the letter to capitalize in a token.
Deliberately excludes `~', `=', `/', `#' and `+', so that paths, Org
verbatim markup and `#+begin_src' lines are left alone.")

(defconst night/h-sentence-case-closing-regexp
  "[]\"')}’”»]+\\'"
  "Trailing punctuation stripped before deciding whether a token ends a sentence.
Note that `]' has to come first to be literal inside the character class.")

(defconst night/h-sentence-case-marker-regexp
  "\\`\\(?:[-*+>#=|]+\\|[0-9]+[.)]\\)\\'"
  "A token that is nothing but a Markdown/Org list or quote marker.
Such tokens are transparent: they are neither capitalized themselves nor
do they consume a pending capitalization.")

(defconst night/h-sentence-case-code-regexp
  "[/\\@$~_`]\\|[[:alnum:]]\\.[[:alnum:]]"
  "A token holding this is treated as code rather than prose.
Matches URLs, paths, snake_case, backticked code, dotted names such as
`file.el', and version numbers such as `1.2.3'.")

(defun night/h-sentence-case-letter-p (char)
  "Return non-nil when CHAR is an alphabetic character."
  (string-match-p "\\`[[:alpha:]]\\'" (char-to-string char)))

(defun night/h-sentence-case-whitespace-p (char)
  "Return t when CHAR is whitespace, nil otherwise."
  (and (memq char '(?\s ?\t ?\n ?\r ?\f)) t))

(defun night/h-sentence-case-code-token-p (token)
  "Return non-nil when TOKEN looks like code rather than prose."
  (string-match-p night/h-sentence-case-code-regexp token))

(defun night/h-sentence-case-marker-p (token)
  "Return non-nil when TOKEN is only a list or quote marker."
  (string-match-p night/h-sentence-case-marker-regexp token))

(defun night/h-sentence-case-mixed-case-p (token)
  "Return non-nil when TOKEN has an uppercase letter past its first character.
This is what keeps `iPhone', `eBay' and `ID' intact."
  (let ((case-fold-search nil))
    (and (> (length token) 1)
         (string-match-p "[[:upper:]]" (substring token 1)))))

(defun night/h-sentence-case-strip-closers (token)
  "Return TOKEN without its trailing quotes and brackets."
  (replace-regexp-in-string night/h-sentence-case-closing-regexp "" token))

(defun night/h-sentence-case-abbreviation-p (token)
  "Return non-nil when TOKEN is in `night/sentence-case-abbreviations'.
Trailing punctuation other than the abbreviation's own dot is ignored,
so `i.e.,' still counts."
  (member (downcase (replace-regexp-in-string "[^[:alnum:].]+\\'" "" token))
          night/sentence-case-abbreviations))

(defun night/h-sentence-case-ends-sentence-p (token)
  "Return non-nil when TOKEN ends a sentence.

Only the end of a token is ever examined, so a dot inside a word -- in
`i.e.', `~/.claude/bin', `a.com' or `1.2.3' -- can never end a sentence.
A trailing dot is discounted for a known abbreviation, for a
single-letter initial such as `J.', and for dotted forms such as
`U.S.A.'."
  (let ((core (night/h-sentence-case-strip-closers token)))
    (cond
     ((string-match-p "[?!]\\'" core)
      t)
     ((string-match-p "\\.\\'" core)
      (let ((base (replace-regexp-in-string "\\.+\\'" "" core)))
        (not
         (or (night/h-sentence-case-abbreviation-p core)
             (string-match-p "\\`[[:alpha:]]\\'" base)
             (string-match-p "\\`[[:alpha:]]\\(?:\\.[[:alpha:]]\\)+\\'" base)))))
     (t nil))))

(defun night/h-sentence-case-capitalize-token (token)
  "Return TOKEN with its first letter uppercased.
Any leading opening punctuation is skipped, so `**bold' and `\"quoted'
are still capitalized."
  (let ((index 0)
        (length (length token)))
    (while (and (< index length)
                (string-match-p night/h-sentence-case-opening-regexp
                                (string (aref token index))))
      (setq index (1+ index)))
    (cond
     ((and (< index length)
           (night/h-sentence-case-letter-p (aref token index)))
      (concat (substring token 0 index)
              (upcase (string (aref token index)))
              (substring token (1+ index))))
     (t token))))

(defun night/h-sentence-case-tokenize (text)
  "Split TEXT into an ordered list of (KIND . STRING) cells.
KIND is `gap' for a run of whitespace and `tok' for a run of anything
else.  Concatenating the strings in order reproduces TEXT exactly."
  (let ((index 0)
        (length (length text))
        (parts nil))
    (while (< index length)
      (let ((gap-p (night/h-sentence-case-whitespace-p (aref text index)))
            (end index))
        (while (and (< end length)
                    (eq gap-p (night/h-sentence-case-whitespace-p (aref text end))))
          (setq end (1+ end)))
        (push (cons (cond (gap-p 'gap) (t 'tok))
                    (substring text index end))
              parts)
        (setq index end)))
    (nreverse parts)))

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

(defun night/h-sentence-case-replace-in-token (token replacements)
  "Apply REPLACEMENTS to whole words in TOKEN.

Applying this per token rather than to the whole text is what keeps the
replacements out of code: the caller never hands us a protected token.
That also settles an old inconsistency, since `\\_<' and `\\_>' resolve
against the calling buffer's syntax table, and `.' is a symbol
constituent in some major modes but not others."
  (let ((case-fold-search t)
        (result token))
    (dolist (replacement replacements result)
      (let ((source (car replacement))
            (target (cdr replacement)))
        (setq result
              (replace-regexp-in-string
               (concat "\\_<"
                       ;; (regexp-quote source)
                       ;; actually, it's better to support regex
                       source
                       "\\_>")
               (lambda (match)
                 (night/h-sentence-case-replacement-target match target))
               result
               nil
               nil))))))

(defun night/h-sentence-case-shouted-p (parts)
  "Return non-nil when the prose tokens of PARTS have letters but no lowercase.
Code tokens are ignored, so `README.md' does not keep an otherwise
all-caps line from being recognized as shouted."
  (night/h-sentence-case-all-uppercase-p
   (mapconcat #'cdr
              (cl-remove-if-not
               (lambda (part)
                 (and (eq (car part) 'tok)
                      (not (night/h-sentence-case-code-token-p (cdr part)))))
               parts)
              " ")))

(defun night/h-sentence-case-transform (text replacements-p)
  "Return TEXT with sentence-starting words capitalized.

Works one whitespace-delimited token at a time.  A token that looks like
code, or that carries an uppercase letter past its first character, is
protected: it gets neither capitalized nor rewritten by the
replacements.  A token that is only a list or quote marker is
transparent, so the word after it still starts the sentence."
  (let* ((parts (night/h-sentence-case-tokenize text))
         (shouted-p (night/h-sentence-case-shouted-p parts))
         (capitalize-next-p t)
         (result nil))
    (dolist (part parts)
      (cond
       ((eq (car part) 'gap)
        (push (cdr part) result)
        (cond
         ((string-match-p "\n" (cdr part))
          (setq capitalize-next-p t))))
       (t
        (let* ((raw (cdr part))
               (code-p (night/h-sentence-case-code-token-p raw))
               (token
                (cond
                 ((and shouted-p (not code-p))
                  (downcase raw))
                 (t raw)))
               (abbreviation-p
                (night/h-sentence-case-abbreviation-p token))
               (protected-p
                (and (not abbreviation-p)
                     (or code-p
                         (night/h-sentence-case-mixed-case-p token)))))
          (cond
           ((night/h-sentence-case-marker-p token)
            (push token result))
           (t
            ;; An abbreviation is prose, so it may be capitalized at the
            ;; start of a sentence, but it is never rewritten -- otherwise
            ;; the standalone `i' rule would turn "i.e." into "I.e." in the
            ;; middle of a sentence.
            (cond
             ((not (or protected-p abbreviation-p))
              (setq token
                    (night/h-sentence-case-replace-in-token
                     token
                     night/sentence-case-always-replacements))
              (cond
               (replacements-p
                (setq token
                      (night/h-sentence-case-replace-in-token
                       token
                       night/sentence-case-replacements))))))
            (cond
             ((and capitalize-next-p (not protected-p))
              (setq token (night/h-sentence-case-capitalize-token token))))
            (setq capitalize-next-p
                  (night/h-sentence-case-ends-sentence-p token))
            (push token result)))))))
    (apply #'concat (nreverse result))))

;;;###autoload
(cl-defun night/sentence-case (text &key insert-p (replacements-p night/sentence-case-enable-replacements))
  "Sentence-case TEXT.

Apply optional whole-word replacements, then capitalize sentence-starting
words while leaving existing non-start case untouched.  When TEXT has
letters but no lowercase letters, downcase it first and then sentence-case it.

Tokens that look like code -- URLs, paths, snake_case, backticked code,
dotted names such as `file.el' -- and tokens carrying an uppercase letter
past their first character, such as `iPhone' or `ID', are left exactly as
they are by both passes.  A dot only ends a sentence at the end of a
token, so `i.e.' and `~/.claude/bin' survive intact, and a trailing dot
on an abbreviation from `night/sentence-case-abbreviations' does not end
one either.

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

(comment
 ;; Every case is checked under two major modes, because `\_<' and `\_>'
 ;; resolve against the calling buffer's syntax table and used to make the
 ;; result depend on where the paste happened.
 (defun night/h-test-sentence-case (input)
   "Return the sentence-cased INPUT, or a mismatch report across major modes."
   (let ((results
          (mapcar
           (lambda (mode)
             (with-temp-buffer
               (funcall mode)
               (night/sentence-case input)))
           '(org-mode text-mode sh-mode))))
     (cond
      ((cl-every (lambda (result) (equal result (car results))) results)
       (car results))
      (t (cons 'mode-dependent results)))))

 (ert-deftest night/sentence-case-leaves-dotted-abbreviations-alone ()
   (should (equal (night/h-test-sentence-case "see i.e. now")
                  "See i.e. now"))
   (should (equal (night/h-test-sentence-case "the U.S.A. is big")
                  "The U.S.A. is big"))
   (should (equal (night/h-test-sentence-case "J. R. R. Tolkien wrote it. good")
                  "J. R. R. Tolkien wrote it. Good")))

 (ert-deftest night/sentence-case-knows-abbreviations ()
   (should (equal (night/h-test-sentence-case "see e.g. the docs. also cf. this")
                  "See e.g. the docs. Also cf. this"))
   (should (equal (night/h-test-sentence-case "i.e., append ~/.claude/bin first")
                  "I.e., append ~/.claude/bin first"))
   (should (equal (night/h-test-sentence-case "cats, dogs, etc. are fine")
                  "Cats, dogs, etc. are fine"))
   ;; An abbreviation that really does end a sentence loses the next
   ;; capital. Documented tradeoff, asserted so the change is deliberate.
   (should (equal (night/h-test-sentence-case "cats, dogs, etc. then i left")
                  "Cats, dogs, etc. then I left")))

 (ert-deftest night/sentence-case-leaves-code-tokens-alone ()
   (should (equal (night/h-test-sentence-case "visit https://a.com/x now. ok")
                  "Visit https://a.com/x now. Ok"))
   (should (equal (night/h-test-sentence-case "https://a.com is up")
                  "https://a.com is up"))
   (should (equal (night/h-test-sentence-case "file.el is ok")
                  "file.el is ok"))
   (should (equal (night/h-test-sentence-case "version 1.2.3 works")
                  "Version 1.2.3 works"))
   (should (equal (night/h-test-sentence-case "mail a@b.com now. ok")
                  "Mail a@b.com now. Ok"))
   (should (equal (night/h-test-sentence-case "#+begin_src elisp")
                  "#+begin_src elisp"))
   (should (equal (night/h-test-sentence-case "=verbatim= start here")
                  "=verbatim= start here"))
   (should (equal (night/h-test-sentence-case "run `dont` please")
                  "Run `dont` please"))
   (should (equal (night/h-test-sentence-case "i know the ID and user_id here")
                  "I know the ID and user_id here")))

 (ert-deftest night/sentence-case-does-not-leak-past-a-code-token ()
   (should (equal (night/h-test-sentence-case "~/.claude/bin is first. done")
                  "~/.claude/bin is first. Done"))
   (should (equal (night/h-test-sentence-case "done.next thing")
                  "done.next thing")))

 (ert-deftest night/sentence-case-handles-markers-and-punctuation ()
   (should (equal (night/h-test-sentence-case "- hello\n> quoted start")
                  "- Hello\n> Quoted start"))
   (should (equal (night/h-test-sentence-case "1. first item\n2. second item")
                  "1. First item\n2. Second item"))
   (should (equal (night/h-test-sentence-case "**bold start** here")
                  "**Bold start** here"))
   (should (equal (night/h-test-sentence-case "(parenthetical start) and more")
                  "(Parenthetical start) and more"))
   (should (equal (night/h-test-sentence-case "he said \"hi.\" then left")
                  "He said \"hi.\" Then left"))
   (should (equal (night/h-test-sentence-case "wait... ok then")
                  "Wait... Ok then")))

 (ert-deftest night/sentence-case-keeps-documented-behavior ()
   (should (equal (night/h-test-sentence-case "hello. world?")
                  "Hello. World?"))
   (should (equal (night/h-test-sentence-case "hello iPhone. use API")
                  "Hello iPhone. Use API"))
   (should (equal (night/h-test-sentence-case "i think i can. i really do")
                  "I think I can. I really do"))
   (should (equal (night/h-test-sentence-case "whats up? dont use sth")
                  "What's up? Don't use something"))
   (should (equal (night/h-test-sentence-case "pls dont do that tho")
                  "Please don't do that though"))
   (should (equal (night/h-test-sentence-case "theyre sure itll work")
                  "They're sure it'll work"))
   (should (equal (night/sentence-case "whats up" :replacements-p nil)
                  "Whats up"))
   (should (equal (night/sentence-case "i know whats up" :replacements-p nil)
                  "I know whats up")))

 (ert-deftest night/sentence-case-downcases-shouted-prose-only ()
   (should (equal (night/h-test-sentence-case "HELLO WORLD. HOW ARE YOU?")
                  "Hello world. How are you?"))
   (should (equal (night/h-test-sentence-case "SEE THE README.md FILE")
                  "See the README.md file")))

 (ert-run-tests-batch "night/sentence-case"))

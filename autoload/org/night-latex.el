;;; autoload/org/night-latex.el -*- lexical-binding: t; -*-

(after! org
  ;; You can adapt the old code at http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/ to automatically change the previews to code and vice versa when the cursor enters/leaves them.
  ;; Update: we already have automatic previews ...
;;;
  (setq org-startup-with-latex-preview
        ;; t
        ;; [[id:86053cea-abb2-43fb-b1d9-8bec1b93286c][elisp: hook: check if buffer has been opened interactively]]
        nil
        )
  (setq org-preview-latex-default-process 'dvisvgm)

;;;
  (defun night/org-format-latex-header-add (line)
    "Append LINE to `org-format-latex-header' unless already present.

Idempotent, so reloading this file never duplicates a package. Note
that `org-format-latex-header' is part of the preview cache hash (see
[agfi:night/h-olpl-cache-file] and
=DOOMDIR/docs/org/latex-preview/performance.md=): editing it
invalidates every cached preview image, so all fragments recompile on
their next preview and the old images become orphans in the shared
cache directory."
    (unless (string-match-p (regexp-quote line) org-format-latex-header)
      (setq org-format-latex-header
            (concat org-format-latex-header "\n" line "\n"))))

  ;; Add preamble lines for the previews here, one call each:
  (night/org-format-latex-header-add
   "\\usepackage[bb=boondox]{mathalpha}") ;; fixes `\mathbb{1}'
;;;
  (defconst night/h-org-latex-display-math-open-re
    "\\(?:^\\|[^\\\\]\\)\\(\\\\\\[\\)"
    "Matches an unescaped display-math opener `\\['.

The leading alternation exists to reject the LaTeX row-break-with-spacing
idiom `\\\\[2ex]', whose bracket is preceded by a second backslash.
Group 1 is the delimiter itself.")

  (defconst night/h-org-latex-display-math-close-re
    "\\(?:^\\|[^\\\\]\\)\\(\\\\\\]\\)"
    "Matches an unescaped display-math closer `\\]'.
See `night/h-org-latex-display-math-open-re'.")

  (defconst night/h-org-latex-display-math-skip-types
    '(src-block example-block export-block comment-block comment fixed-width
      keyword code verbatim link latex-environment
      table table-row table-cell)
    "Element/object types whose `\\[...\\]' must never be rewritten.

Code, verbatim and comments include documentation *about* this bug, and a
multi-line environment inside a table cell would smear across rows.")

  (defun night/h-org-latex-display-math-broken-p (open-beg close-end)
    "Return non-nil when Org mis-parses the display math at OPEN-BEG..CLOSE-END.

Decided by asking `org-element-context' rather than by re-implementing
Org's terminator rules, so this also covers the sibling failure modes
where a lone `+'/`-'/`=' line splits the paragraph (see
=~/scripts/docs/md2org-latex/readme.md=)."
    (save-excursion
      (goto-char open-beg)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        (cond
         ((memq type night/h-org-latex-display-math-skip-types)
          nil)
         ((and (eq type 'latex-fragment)
               (>= (org-element-property :end context) close-end))
          ;; Already parses as one fragment, so it previews and exports fine.
          nil)
         (t t)))))

  (defun night/h-org-latex-delimiter-isolate (beg end replacement)
    "Replace BEG..END with REPLACEMENT, alone on a line of its own.

Org only recognizes `\\begin{...}'/`\\end{...}' at the start of a line
\(leading whitespace is allowed), so any text sharing the line with the
old delimiter is pushed onto the previous or next line, indented like the
delimiter's own line."
    (let* ((indent (save-excursion
                     (goto-char beg)
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (progn (back-to-indentation) (point)))))
           (head-p (save-excursion
                     (goto-char beg)
                     (string-match-p
                      "[^ \t]"
                      (buffer-substring-no-properties
                       (line-beginning-position) beg))))
           (tail-p (save-excursion
                     (goto-char end)
                     (string-match-p
                      "[^ \t]"
                      (buffer-substring-no-properties
                       end (line-end-position))))))
      ;; Absorb the whitespace that separated the delimiter from its
      ;; line-mates, so the split does not leave a dangling space behind.
      ;; Only when there *are* line-mates: otherwise this would eat the
      ;; indentation of e.g. math inside a list item.
      (when head-p
        (setq beg (save-excursion (goto-char beg) (skip-chars-backward " \t") (point))))
      (when tail-p
        (setq end (save-excursion (goto-char end) (skip-chars-forward " \t") (point))))
      (delete-region beg end)
      (goto-char beg)
      (when head-p
        (insert "\n" indent))
      (insert replacement)
      (when tail-p
        (insert "\n" indent))))

  (defun night/h-org-latex-display-math-broken-blocks (beg end)
    "Return the `\\[...\\]' blocks in BEG..END that Org mis-parses.

Each element is (OPEN-BEG OPEN-END CLOSE-BEG CLOSE-END), ordered last
block first so that rewriting them in sequence keeps earlier positions
valid."
    (let ((blocks nil))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward night/h-org-latex-display-math-open-re end t)
          (let ((open-beg (match-beginning 1))
                (open-end (match-end 1)))
            (goto-char open-end)
            (cond
             ((re-search-forward night/h-org-latex-display-math-close-re end t)
              (let ((close-beg (match-beginning 1))
                    (close-end (match-end 1)))
                (when (and
                       ;; Single-line blocks cannot hit the bug.
                       (save-excursion
                         (goto-char open-beg)
                         (< (line-end-position) close-beg))
                       (night/h-org-latex-display-math-broken-p open-beg close-end))
                  (push (list open-beg open-end close-beg close-end) blocks))
                (goto-char close-end)))
             (t
              ;; Unterminated opener: nothing further can be matched.
              (goto-char end))))))
      blocks))

  (defun night/org-latex-fix-begin-env-bug (&optional beg end)
    "Rewrite display math that Org's parser breaks on, in BEG..END.

Interactively this is the region, or the whole buffer when there is none.

A line starting with `\\begin{...}' opens a `latex-environment' *element*
\(`org-element--latex-begin-environment' has no `$' anchor), which
terminates the enclosing paragraph. A `\\[...\\]' block containing such a
line therefore loses its closing delimiter to the next element: the `\\['
degrades to plain text and only the inner environment is previewed or
exported. See =DOOMDIR/docs/org/latex-preview/begin-env-bug.md=.

Affected blocks are rewritten to `\\begin{equation*}...\\end{equation*}',
which Org parses line-wise and is therefore immune to any interior
paragraph break. This is the same normal form that
=$NIGHTDIR/python/pandoc_filters/org_math_env.lua= produces for the
[help:night/paste-md2org] path, so text arriving by either route ends up
identical. Blocks that already parse correctly are left untouched, which
makes the command idempotent."
    (interactive
     (cond
      ((use-region-p) (list (region-beginning) (region-end)))
      (t (list nil nil))))
    (let* ((beg (or beg (point-min)))
           (end (or end (point-max)))
           (blocks (night/h-org-latex-display-math-broken-blocks beg end))
           (count (length blocks)))
      (save-excursion
        (pcase-dolist (`(,open-beg ,open-end ,close-beg ,close-end) blocks)
          ;; Closer first: rewriting it cannot move the opener.
          (night/h-org-latex-delimiter-isolate
           close-beg close-end "\\end{equation*}")
          (night/h-org-latex-delimiter-isolate
           open-beg open-end "\\begin{equation*}")))
      (when (called-interactively-p 'any)
        (message "night/org-latex-fix-begin-env-bug: rewrote %d block(s)" count))
      count))
;;;

  ;; https://emacs.stackexchange.com/questions/19880/font-size-control-of-latex-previews-in-org-files
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3.0))
  (setq org-format-latex-options (plist-put org-format-latex-options :foreground "black"))

  (defun night/latex-syntax-highlighting-enable ()
    (interactive)
    (setq org-latex-listings 'minted)

    (setq org-latex-custom-lang-environments
          '(
            (emacs-lisp "common-lispcode")))

    (setq org-latex-minted-options
          '(("frame" "lines")
            ("fontsize" "\\scriptsize")
            ("linenos" "")))

    (setq org-latex-to-pdf-process
;;;
          ;; '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          ;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          ;;   "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
;;;
          ;; '("latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
;;;
          ;; I tried using an elisp function here, but it errored. The docs seem to be wrong here.

          ;; '("brishzq.zsh h-pdflatex-emacs %F")
          '("brishzq.zsh h-pdflatex-emacs-async %F")
;;;
          )
    ;; `org-latex-to-pdf-process' was probably deprecated.
    (setq org-latex-pdf-process org-latex-to-pdf-process)
    ;; -shell-escape  Enable the \write18{command} construct. The command can be any shell  command. This construct is normally disallowed for security reasons.
    )
  (night/latex-syntax-highlighting-enable)
  )

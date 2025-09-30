;;; autoload/git/night-git-worktrees.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)

(defcustom night/worktree-selection-function nil
  "Reader used to select a worktree from candidates.

If nil, prefer `consult--read' when available, else fallback to
`completing-read'. The function is called as:

  (FUNC PROMPT CANDIDATES DEFAULT)

and must return the chosen candidate (a string)."
  :type '(choice (const :tag "Auto" nil) function))

(defcustom night/worktree-sort-key 'name
  "How to sort worktree candidates.
One of: `name', `branch', `path', or nil (keep Git's order)."
  :type '(choice (const :tag "Name (alphabetical)" name)
                 (const :tag "Branch (alphabetical)" branch)
                 (const :tag "Path (alphabetical)" path)
                 (const :tag "Git order (no sort)" nil)))

(cl-defun night/git-worktree-switch (&key selector git root)
  "List worktrees and visit the current file in the chosen one.

Prompts for a worktree belonging to the repo that contains the
current buffer, then visits the same relative file path there.

Optional keyword args:

- `:selector' Reader fn (PROMPT CANDS DEFAULT) → STRING.
- `:git'      Path to the git executable (defaults to `night/git-executable').
- `:root'     Repo root to operate in."
  (interactive)
  (let ((verbosity-level 0))
    (condition-case err
        (let* ((night/git-executable (or git night/git-executable))
               (repo-root (or root (night/h--git-toplevel default-directory)))
               (file buffer-file-name))
          (cond
           ((null file)
            (user-error "This buffer is not visiting a file"))
           ((not (file-exists-p repo-root))
            (user-error "Not inside a git repository"))
           ((not (file-in-directory-p file repo-root))
            (user-error "Current file is not inside the repository at %s" repo-root))
           (t
            (let* ((lines (night/h--git-lines repo-root "worktree" "list" "--porcelain"))
                   (als
                    (let (acc current)
                      (dolist (ln lines)
                        (cond
                         ((string-prefix-p "worktree " ln)
                          (when current (push (nreverse current) acc))
                          (setq current (list (cons :path (string-remove-prefix "worktree " ln)))))
                         ((string-prefix-p "HEAD " ln)
                          (push (cons :head (string-remove-prefix "HEAD " ln)) current))
                         ((string-prefix-p "branch " ln)
                          (let* ((ref (string-remove-prefix "branch " ln))
                                 (name (string-remove-prefix "refs/heads/" ref)))
                            (push (cons :branch name) current)))
                         ((string-prefix-p "detached" ln) (push (cons :detached t) current))
                         ((string-prefix-p "bare" ln)      (push (cons :bare t) current))
                         ((string-prefix-p "locked" ln)    (push (cons :locked t) current))
                         ((string-prefix-p "prunable" ln)  (push (cons :prunable t) current))
                         (t nil)))
                      (when current (push (nreverse current) acc))
                      (nreverse acc)))
                   (fmt
                    (lambda (al)
                      (let* ((path (alist-get :path al))
                             (name (file-name-nondirectory (directory-file-name path)))
                             (branch (alist-get :branch al))
                             (det (alist-get :detached al)))
                        (concat name
                                (cond
                                 (branch (format " [%s]" branch))
                                 (det " [detached]")
                                 (t ""))
                                " — " path))))
                   (items (mapcar (lambda (al)
                                    (list :path (alist-get :path al)
                                          :branch (alist-get :branch al)
                                          :head (alist-get :head al)
                                          :display (funcall fmt al)))
                                  als)))
              (cond
               ((null items)
                (user-error "No git worktrees found"))
               (t
                (cl-labels
                    ((currentp (it)
                       (string-equal (file-name-as-directory repo-root)
                                     (file-name-as-directory (plist-get it :path))))
                     (key-of (it)
                       (cond
                        ((eq night/worktree-sort-key 'name)
                         (file-name-nondirectory (directory-file-name (plist-get it :path))))
                        ((eq night/worktree-sort-key 'branch)
                         (or (plist-get it :branch) "~"))
                        ((eq night/worktree-sort-key 'path)
                         (plist-get it :path))
                        (t ""))) ;; for nil (no sort)
                     (sort-items (xs)
                       (cond
                        ((null night/worktree-sort-key) xs)
                        (t (cl-sort (copy-seq xs) #'string-lessp :key #'key-of)))))
                  (let* ((sorted (sort-items items))
                         (displays (mapcar (lambda (x) (plist-get x :display)) sorted))
                         (by-display (mapcar (lambda (x) (cons (plist-get x :display) x)) sorted))
                         (default-item (cl-find-if (lambda (x) (not (currentp x))) sorted))
                         (default-display (and default-item (plist-get default-item :display)))
                         (reader
                          (cond
                           ((functionp selector) selector)
                           ((functionp night/worktree-selection-function) night/worktree-selection-function)
                           ((fboundp 'consult--read)
                            (lambda (prompt candidates default)
                              (consult--read candidates
                                             :prompt prompt
                                             :require-match t
                                             :default default)))
                           (t (lambda (prompt candidates default)
                                (completing-read prompt candidates nil t nil nil default)))))
                         (chosen (funcall reader "Worktree: " displays default-display)))
                    (when (or (null chosen) (string-empty-p chosen))
                      (user-error "No worktree selected"))
                    (let* ((target (plist-get (cdr (assoc chosen by-display)) :path))
                           (rel (file-relative-name file repo-root))
                           (dest (expand-file-name rel target)))
                      (when (> verbosity-level 0)
                        (message "Visiting %s in worktree %s" rel target))
                      (make-directory (file-name-directory dest) t)
                      (find-file dest)
                      dest)))))))))
      (quit
       (message "night/git-worktree-switch: quit")
       nil)
      (error
       (message "night/git-worktree-switch error: %s" (error-message-string err))
       nil))))

(map! :leader "gw" #'night/git-worktree-switch)
;;;

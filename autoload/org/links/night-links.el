;;; autoload/org/night-links.el -*- lexical-binding: t; -*-

(after! (org evil-org evil ol)
;;;
  (cl-defun night/org-show-link-display (&key (hide nil) (default t))
    (interactive)
    (when default
      (setq-default org-link-descriptive hide))

    (setq org-link-descriptive (not hide))
    ;; will become buffer-local and toggled next

    (org-toggle-link-display))

  (cl-defun night/org-hide-link-display (&key (default t))
    (interactive)
    (night/org-show-link-display :hide t :default default))

  (night/org-show-link-display) ;; this will cause the links to be displayed fully on startup:

;;;
  ;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive)

  (defun night/org-title (&optional file-path)
    "Returns the string used by default for the current org file's title."
    (interactive)
    (let* (
           (bfn (cond
                 (file-path
                  (or
                   (cadr (s-match "^[^/]*:\\(.*\\)" file-path))
                   file-path))
                 (t (buffer-file-name))))
           (fb (f-base bfn))
           (magic_marker "MAGIC_2720091660_")
           (slash_or_marker (concat  "\\(" magic_marker "\\|/\\|$\\)"))
           (preserve (lambda (str n)
                       (let* ((after
                               (replace-regexp-in-string
                                (concat slash_or_marker "\\(" n "\\)" slash_or_marker)
                                (concat magic_marker "\\2\\3") str)))
                         ;; (message "before: %s\nafter: %s" str after)
                         after)))
           (parent (cond
                    (file-path
                     (cadr (s-match "^\\([^/]*:\\)" file-path)))
                    (t "")))
           (parent-2
            (or (ignore-errors ;; with-demoted-errors ;; ignore-errors
                  (--> bfn
                    (funcall preserve it "resources?")
                    (funcall preserve it "learn")
                    (funcall preserve it "cheatsheets?")
                    (funcall preserve it "gen\\(?:eral\\)?")
                    (funcall preserve it "books?")
                    (funcall preserve it "papers?")
                    (funcall preserve it "practical")
                    (funcall preserve it "interactive")
                    (funcall preserve it "libraries")
                    (funcall preserve it "extensions?")
                    (funcall preserve it "x")
                    (funcall preserve it "todos?")
                    (funcall preserve it "ideas?")
                    (prog1 (f-dirname it)
                      (message "it: %s" it))
                    (f-base it)
                    (replace-regexp-in-string magic_marker "/" it)))
                ""))
           (parent (cond
                    ((not (or (equalp parent-2 "")
                              (equalp parent-2 ".")))
                     (concat parent-2 "/"))
                    (t parent))))
      (if (and parent
               ;; (< 7 (length fb))
               )
          (concat parent fb)
        fb)))
  (comment
   (f-base "HOME:.zshrc")
   (f-base ".zshrc")
   (f-filename "HOME:.zshrc")
   (night/org-title "HOME:.zshrc")
   (night/org-title "HOME:b/.zshrc")
   (night/org-title "HOME:b/a/.zshrc"))

  (defun night/org-description-formatter (link desc)
    (message "%s" (concat "night/org-description-formatter" " (beg): link=" link ", desc=" desc))
    (with-demoted-errors "Error in night/org-description-formatter: %S"
      (let* (
             (file
              (if (s-starts-with? "id:" link t)
                  (car (org-id-find (s-chop-prefix "id:" link)))
                link))
             (parent
              (if file
                  (f-filename (f-dirname file))
                nil))
             (parent (if parent
                         (or
                          (cadr (s-match "^[^/]*:\\(.*\\)" parent))
                          parent)
                       nil)) ;;  we can run this on file, as well, or `HOME:.xsh` links won't get their prefix stripped. But I think that's okay.
             (tail (cond
                    (file (night/org-title file)
                          ;; we can concat a '.org' here
                          )
                    (t nil)))
             (tail
              (cond
               ((and tail (not (equalp tail "")))
                (prog1 tail
                  (message "tail from night/org-title: %s" tail)))
               ((or (equalp "." parent) (equalp "" parent))
                file)
               ((and parent (not (equalp parent ""))) (concat
                                                       parent
                                                       "/"
                                                       (f-filename file)))
               (file (f-filename file))
               (t "")))
             (desc (when desc (-> desc
                                (string-trim-right ":?\\*?links\\*?")
                                (string-trim-left "\\*+\s+")
                                ;; (night/org-str-to-plain)
                                ))))
        (message "%s" (concat "night/org-description-formatter: " "link=" link ", desc=" desc ", tail=" tail ", file=" file))
        (cond
         ((and desc (not (equalp desc "")) (equalp desc tail)) desc)
         ((and desc (not (equalp desc "")) tail (not (equalp tail "")))
          (let ((desc-tail (or
                            (cadr (s-match ".*:\\(.*\\)" desc))
                            desc)))
            (if (not (equalp  desc-tail ""))
                (concat tail ":" desc-tail)
              tail)))
         ((and tail (not (equalp tail ""))) tail)
         ((and desc (not (equalp desc ""))) desc)
         (t link)))))

  ;; (cadr (s-match "^[^/]*:\\(.*\\)" "aJK:lol/as"))
  ;; (org-id-find "124fa3e8-c032-4b80-9ede-d3caff9cec8a")

  (setq org-link-make-description-function #'night/org-description-formatter)

  (require 'org-super-links)
  (after! org-super-links
    (setq org-super-links-backlink-prefix nil)
    (setq org-super-links-link-prefix nil)
    (setq org-super-links-link-postfix nil)
    (setq org-super-links-default-description-formatter #'night/org-description-formatter)

    (map!
     :map org-mode-map
     :localleader
     ;; overrides the default link bindings
     "ls" #'org-super-links-store-link
     "lS" #'org-super-links-insert-link
     "ld" #'org-super-links-delete-link
     ;; "la" #'org-super-links-quick-insert-drawer-link
     )
    )
;;;
  (setcdr org-link-abbrev-alist
          `(
            ("HOME" . ,(concat (getenv "HOME") "/"))
            ("DOOMDIR" . ,(concat (getenv "DOOMDIR") "/"))
            ("NIGHTDIR" . ,(concat (getenv "NIGHTDIR") "/"))
            ("cellar" . ,(concat (getenv "cellar") "/"))
            ("nightNotes" . ,(concat (getenv "nightNotes") "/"))
            ("orgdir" . ,(concat  org-directory "/"))
            ("vol" . ,(concat  "/Volumes/"))))
  (add-to-list 'org-modules 'org-protocol)
  ;; https://orgmode.org/manual/Link-Abbreviations.html
  ;; If you need special abbreviations just for a single Org buffer, you can define them in the file with:
  ;; #+LINK: google    http://www.google.com/search?q=%s


  (defun org-nightNotes-complete-link ()
    (concat "nightNotes:" (night/browse-notes))
    )
  ;; @bug setting these completion functions causes these link types to appear twice in the =\ l l= list
  (org-link-set-parameters "nightNotes" :complete #'org-nightNotes-complete-link)

  (defun org-NIGHTDIR-complete-link ()
    (concat "NIGHTDIR:" (night/browse-NIGHTDIR))
    )
  (org-link-set-parameters "NIGHTDIR" :complete #'org-NIGHTDIR-complete-link)

  (defun org-DOOMDIR-complete-link ()
    (concat "DOOMDIR:" (night/browse-DOOMDIR))
    )
  (org-link-set-parameters "DOOMDIR" :complete #'org-DOOMDIR-complete-link)

  (defun org-HOME-complete-link ()
    (concat "HOME:" (night/browse-HOME))
    )
  (org-link-set-parameters "HOME" :complete #'org-HOME-complete-link)
;;;
  (after! (org-yt)
    (defun org-yt-follow (video-id)
      "Open youtube with VIDEO-ID."
      (z awaysh mpv-notag --force-window=immediate (concat "https://youtu.be/" video-id))))
;;;
  (defun night/org-remove-link-to-desc-at-point ()
    "Replace an org link by its description or if empty its address"
    ;; Forked from https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
    (interactive)
    (if (org-in-regexp org-link-bracket-re 1)
        (save-excursion
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description
                 (if (match-end 2)
                     (org-match-string-no-properties 2)
                   (org-match-string-no-properties 1))))
            (apply 'delete-region remove)
            (insert description)))))

  (defun night/org-remove-link-to-desc (beg end)
    (interactive "r")
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward org-link-bracket-re end t)
        (goto-char (match-beginning 0))
        (night/org-remove-link-to-desc-at-point))))
;;;
  )

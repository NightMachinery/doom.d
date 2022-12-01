;;; autoload/night-zsh-wrappers.el -*- lexical-binding: t; -*-
;;;
(defun night/org-link-browser-current ()
  (interactive)
  (night/insert-for-yank-and-save
   (z org-link-browser-current)))
;;;
(defun night/p-org-fanfic ()
  (interactive)
  (night/insert-for-yank-and-save
   (z p-org-fanfic))
  (night/bell-link))
;;;
(defun night/p-newline2space ()
  (interactive)
  (night/insert-for-yank
   (z p-newline2space)))
(defalias 'night/pns #'night/p-newline2space)
;;;
(defun night/semantic-scholar-to-org-sync ()
  (interactive)
  (night/org-insert-and-fix-levels
   (z semantic-scholar-to-org))
  (night/save-buffer)
  (night/bell-link))

(defun night/semantic-scholar-to-org ()
  (interactive)
  (night/brishz-async-insert
   :name "night/semantic-scholar-to-org"
   :command (list "semantic-scholar-to-org")
   :callback-after #'night/bell-link
   :insert-fn #'night/org-insert-and-fix-levels
   :save-p t))
;;;
(require 'ov)
(cl-defun night/ov (&key
                    beg end
                    properties
                    front-advance
                    rear-advance
                    )
  "Make an overlay from BEG to END.

If PROPERTIES are specified, set them for the created overlay."
  (if properties
      (progn
        ;; To pass properties to `ov-set'
        (when (listp (car-safe properties))
          (setq properties (car properties)))
        (let ((o (ov-make beg end nil front-advance rear-advance)))
          (ov-set o properties)
          o))
    (ov-make beg end nil front-advance rear-advance)))

(cl-defun night/brishz-async-insert
    (&key
     command
     (name "night/brishz-async-insert")
     (callback-after #'night/bello)
     (callback-failure #'night/bell-fail)
     (insert-fn night/org-insert-and-fix-levels)
     (save-p t))
  (lexical-let*
      ((default-directory "/") ;; stop remote possible execution
       (callback-after callback-after)
       (insert-fn insert-fn)
       (save-p save-p)
       (name name)
       (overlay nil)
       (current-marker (point-marker))
       (current-buffer (current-buffer))
       (callback
        (lambda (out ret err &rest dummy)
          (when (or
                 (not
                  (equalp ret 0))
                 (not
                  (equalp err "")))
            (message
             "%s: ret: %s\nout: %s\nerr: %s"
             name ret out err))
          (save-excursion
            (with-current-buffer current-buffer
              (goto-char current-marker)
              (delete-overlay overlay)
              (+nav-flash/blink-cursor)
              (funcall insert-fn out)
              (when save-p
                (night/save-buffer :force-normal-state nil))
              (funcall callback-after))))))
    (progn
     (comment
      (+nav-flash/blink-cursor))
     (let
         ((pos (point))
          (start nil)
          (end nil))
       (save-excursion
         (let ((inhibit-point-motion-hooks t)
               (cursor-intangible-mode nil)
               (cursor-sensor-mode nil))
           (goto-char pos)
           (beginning-of-visual-line)
           (setq start (point))
           (end-of-visual-line)
           (setq end (1+ (point)))))
       (setq overlay
             (night/ov
              ;; :beg start
              :beg pos
              :end end
              :properties (list
                           'face
                           'hi-pink
                           'ov-night-async 4999)
              :front-advance nil
              :rear-advance nil
              ;; :front-advance t
              ;; :rear-advance t
              ))))
    (night/brishz-ll
     :name name
     :command command
     :callback callback)))
;;;
(defun night/url2org ()
  (interactive)
  (night/brishz-async-insert
   :name "night/url2org"
   :command (list "url2org-emc")
   :callback-after #'night/bell-link
   :insert-fn #'night/org-insert-and-fix-levels
   :save-p t))
;;;

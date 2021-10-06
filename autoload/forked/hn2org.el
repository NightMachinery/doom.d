;;; hn.el --- convert hacker news post into org-mode buffer  -*- lexical-binding: t -*-
;;;
;; @forked from:
;; - https://gist.github.com/agumonkey/d3eee79840206c28ee9e50d8354a3fad
;;   + @Powershell https://gist.github.com/dharmatech/2ef271907cb73298318fdb50a4ee7d54
;;;
;; Copyright (C) 2018- Free Software Foundation, Inc.

;; Author: Johan Ponin <johan.ponin.pro@gmail.com>
;; Version: 0.0.1
;; Package-Version: 20181103.0001
;; Keywords: hackernews, org-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Given an Hacker News story ID, generates an org-mode buffer of the comments
;;
;; @retired: hn id caching (memoize hn/json)
;;
;; See documentation on <none>
;;
;; **Please note** The lexical binding in this file is not utilised at the
;; moment.  We will take full advantage of lexical binding in an upcoming 3.0
;; release of Dash.  In the meantime, we've added the pragma to avoid a bug that
;; you can read more about in https://github.com/magnars/dash.el/issues/130.
;;

;;; Code:

(require 'dash)
(require 'json)
(require 's)

(defvar *hn/ids-cache* '())

(defvar *hn/json-endpoint* "https://hacker-news.firebaseio.com/v0/item/%s.json?print=pretty")
;; @official https://github.com/HackerNews/API

;;;
(defun night/html2org (html)
  ;; @dep
  (z html2org (make-temp-file (temporary-file-directory) nil nil html))
  ;; (z reval-withstdin
  ;;    (z reval-withstdin (i html)
  ;;       recode ascii..)
  ;;    html2org)
  )
;;; FP
(defun night/map-listy-dict (a k v c)
  "Combinator to map over alist."
  (-map (lambda (p) (funcall c
                             (funcall k (car p))
                             (funcall v (cdr p))))
        a))

(defun walk (r n d c s)
  "Generic walk combinator.
R: Root
N: function applied on a Node
D: function to go Down one level
C: function to combine N(n) with walk over childrens D(n)
S: state variable"
  (funcall c (funcall n r s)
           (let ((ns (1+ s)))
             (-map (lambda (_) (walk _ n d c ns)) (funcall d r)))))

(defun walk! (node xform down io state)
  "Generic walk effectful combinator.
NODE: node being walked
XFORM: function applied on NODE (with STATE)
DOWN: function to generate [child] form NODE
IO: effect function applied on XFORM(NODE, STATE)
STATE: state variable"
  (let ((x (funcall xform node state)))
    (funcall io x)
    (let ((state (1+ state))
          (children (funcall down node)))
      (-each children (lambda (c) (walk! c xform down io state))))))

;;; ALIST

(defalias 'prop 'alist-get)

(defun props (o &rest ps)
  "Helper to project an alist O on a list of keys PS.
ALIST -> ALIST"
  (-zip ps (-map (lambda (p) (prop p o)) ps)))

;;; HTTP

(defun url-retrieve-synchronously:json (u)
  "Fetch JSON from URL U."

  ;;; the returned type doesn't work for us
  ;; (request u
  ;; :parser 'json-read
  ;; :sync t
  ;; :complete (cl-function
  ;;           (lambda (&key data &allow-other-keys)
  ;;             data)))
  ;;;
  (with-temp-buffer
    (url-insert-file-contents u)
    (json-read))
  ;;;
  ;; (with-current-buffer (url-retrieve-synchronously u)
  ;;   ;; (set-buffer-file-coding-system 'utf-8)
  ;;   (set-buffer-multibyte t)
  ;;   (goto-char url-http-end-of-headers)
  ;;   (json-read))
  )
(comment
 (url-retrieve-synchronously:json "https://hacker-news.firebaseio.com/v0/item/28100700.json?print=pretty")

 (request "https://hacker-news.firebaseio.com/v0/item/28100700.json?print=pretty"
  :parser 'json-read
  :sync t
  :complete (cl-function
            (lambda (&key data &allow-other-keys)
              data)))
 )
;;; HN

(defun hn/json (id)
  "Convert HN ID to JSON.
ID: An Hacker news object ID"
  (->> id
       (format *hn/json-endpoint*)
       (url-retrieve-synchronously:json)))

(defun hn/type (o)
  "Accessor for type of O."
  (prop 'type o))

(defun hn/kids (o)
  "Hn object -> kids id list (from json response vector).
O: JSON Object"
  (-map #'identity (cdr (assoc 'kids o))))

;;; FORMAT

(defun hn/format-properties (o s)
  "ALIST a b -> ALIST str str -> str.
O: Alist
S: Stateful variable used for formatting"
  (let* ((indent (indent s ? ))
         (props-list (night/map-listy-dict o
                                           (lambda (k) (format ":%s:" (upcase (format "%S" k))))
                                           (lambda (v) (format "%S" v))
                                           (lambda (a b)
                                             (cond
                                              ((s-matches-p "\s*:TEXT:" a)
                                               nil)
                                              (t (format "%s    %s" a b))))))
         (props-list (-filter #'identity props-list))
         (props (mapconcat #'identity props-list "\n")))
    (format ":PROPERTIES:\n%s\n:END:\n" props)))

(defun hn/format (o s)
  "Format a JSON Object.
O: JSON Object
S: Stateful information"
  (let ((k (hn/type o))
        (indent (indent s ?*))
        (author (prop 'by o))
        (ps (hn/format-properties o s)))
    (cond ((equal k "comment")
           (let* ((props (hn/format-properties o s))
                 (text (prop 'text o))
                 (text (night/html2org text)))
             (s-lex-format "\n${indent} ${author}\n${ps}${text}")))
          ((equal k "story")
           (let ((title (prop 'title o)))
             (s-lex-format "#+TITLE ${title}\n#+AUTHOR ${author}\n${indent} ${title} by ${author}\n${ps}\n")))
          (t (format "[unknown] %S \n" o)))))

;;; HELPERS

(defalias 'indent 'make-string)

;;; MAIN

;;; id -> json -> walk

(defun hn/to-org-mode! (id &optional dest)
  "Downloads a Hacker News story by its ID, converts it to org-mode, and saves the result to `dest'."
  (interactive "sid: \nFdest: ")
  (message "hn/to-org-mode!: id: %s, dest: %s" id dest)
  ;; (setq id "https://news.ycombinator.com/item?id=28689707")
  (let* ((id (elt (s-match "\\?id=\\([[:digit:]]+\\)" id) 1))
         (output (get-buffer-create (format "hn-%s.org" id))))
    (walk! (hn/json id)
           #'hn/format
           (lambda (r) (-map #'hn/json (hn/kids r)))
           (lambda (s)
             (with-current-buffer output
               (insert s)
               (insert "\n")))
           1)
    (with-current-buffer output
      (org-mode)
      (cond
       (dest
        (write-file dest)
        (buffer-file-name))
       (t
        (switch-to-buffer-other-window output))))))

;;; TEST

(defvar *hn-ids*
  '("https://news.ycombinator.com/item?id=18363974"
    "https://news.ycombinator.com/item?id=18370208"
    "https://news.ycombinator.com/item?id=18360847"))

(defun hn/test! ()
  "Test."
  (interactive)
  (hn/to-org-mode! "28091520"))

(comment
 (hn/json "28100700")
 ;; ((by . "anyfoo") (id . 28100700) (kids . [28100868]) (parent . 28097225) (text . "This is the notation that emacs has been using forever, and also displays to the user, so every user should be well comfortable with it. It even says âPress C-x C-c to exit emacsâ at startup.") (time . 1628360486) (type . "comment"))
 )
(provide 'hn2org)
;;; hn2org.el ends here

;;; autoload/night-youtube.el -*- lexical-binding: t; -*-

;;; @forked from https://sachachua.com/blog/2021/04/org-mode-insert-youtube-video-with-separate-captions/
(defun night/msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

(defun night/org-insert-youtube-video-with-transcript (url)
  (interactive "MURL: ")
  (let* ((id (if (string-match "\\(v=\\|://youtu\.be/\\)\\([^&]+\\)" url) (match-string 2 url) url))
         (temp-file (make-temp-name "org-youtube-"))
         (temp-file-name (concat temp-file ".en.srv1"))
         data)
    (if (and (night/brishz "youtube-dl" "--write-sub" "--write-auto-sub"  "--no-warnings" "--sub-lang" "en" "--skip-download" "--sub-format" "srv1"
                           "-o" temp-file
                           (format "https://youtube.com/watch?v=%s" id))
             (file-exists-p temp-file-name))
        (progn (insert
                (format "#+begin_export html
<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/%s\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe>\n#+end_export\n" id)
                "\n"
                (mapconcat (lambda (o)
                             (format "| [[https://youtube.com/watch?v=%s&t=%ss][%s]] | %s |\n"
                                     id
                                     (dom-attr o 'start)
                                     (night/msecs-to-timestamp (* 1000 (string-to-number (dom-attr o 'start))))
                                     (->> (dom-text o)
                                       (replace-regexp-in-string "[ \n]+" " ")
                                       (replace-regexp-in-string "&#39;" "'")
                                       (replace-regexp-in-string "&quot;" "\""))))
                           (dom-by-tag (xml-parse-file temp-file-name) 'text)
                           ""))
               (delete-file temp-file-name))
      (message "failed to download the subtitle to file: %s" temp-file-name)
      )))
;;;

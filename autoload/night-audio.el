;;; autoload/night-audio.el -*- lexical-binding: t; -*-
;;;
(defun night/audio-path-checkable-p (url)
  "Whether URL is a local path we can check for existence from Emacs.

Excludes URLs, and paths starting with a zsh named directory (=~mu/...=,
registered via =hash -d=). `expand-file-name' silently mis-resolves the latter
as relative instead of erroring, so only zsh can resolve them; see the comment
in `night/org-link-zopen-follow'."
  (not (or (string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*://" url)
           (string-match-p "\\`~[^/]" url))))

(cl-defun night/hear (url &key (command
                                "auto"
                                ;; "hear-loadfile-begin"
                                ;; "mpv"
                                ))
  ;; Bail out before handing a missing file to the audio mpv server. `hear-loadfile'
  ;; guards this too (and covers cases Emacs cannot see, e.g. an unmounted volume),
  ;; but we dispatch through `awaysh-oneinstance', so its failure never comes back
  ;; to us. Checking here is what puts something in the echo area.
  (when (and (night/audio-path-checkable-p url)
             (not (file-exists-p url)))
    (night/hs-alert (format "Audio file not found:\n%s" url)
                    :color "warn")
    (message "night/hear: file does not exist: %s" url)
    (cl-return-from night/hear nil))

  (let* ((command
          (cond
            ((equalp command "auto")
             (cond
               ((s-ends-with? ".raw_playlist" url)
                "hear-load-playlist"
                )
               ((s-ends-with? ".m3u" url)
                ;; "hear-load-playlist"
                "hear-playlist")
               (t "hear-loadfile-begin")))
            (t command))))

    (message "Playing audio: %s" url)
;;;
    ;; (night/brishz "awaysh-oneinstance" night/marker-audio "hearinvisible" url)
;;;
    (comment
     ;; @workaround for the lack of support of non-utf-8 in brish
     (eredis-set "emacs_audio_file" url)
     (night/brishz "eval" (concat "awaysh-oneinstance " night/marker-audio " " command "  \"$(redism get emacs_audio_file)\"")))
    (night/brishz "awaysh-oneinstance" night/marker-audio command url)))

(defun night/hearinvisible (url &rest args)
;;; @tests
  (comment
   (night/hearinvisible (z eval print -r -- "$NIGHTDIR/resources/audio/ReichPhase.wav")))
;;;
  (apply #'night/hear url :command "hearinvisible" args))

;;;

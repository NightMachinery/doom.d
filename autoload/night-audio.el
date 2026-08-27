;;; autoload/night-audio.el -*- lexical-binding: t; -*-
;;;
;; `night/path-checkable-p' lives in night-external.el, next to the other path
;; helpers; it is not audio-specific and the org link handlers use it too.

(cl-defun night/hear (url &key (command
                                "auto"
                                ;; "hear-loadfile-begin"
                                ;; "mpv"
                                ))
  ;; Bail out before handing a missing file to the audio mpv server. `hear-loadfile'
  ;; guards this too (and covers cases Emacs cannot see, e.g. an unmounted volume),
  ;; but we dispatch through `awaysh-oneinstance', so its failure never comes back
  ;; to us. Checking here is what puts something in the echo area.
  (when (and (night/path-checkable-p url)
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

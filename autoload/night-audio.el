;;; autoload/night-audio.el -*- lexical-binding: t; -*-
;;;
(cl-defun night/hear (url &key (command
                                "auto"
                                ;; "hear-loadfile-begin"
                                ;; "mpv"
                                ))
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

;;; night-whisper.el ---                             -*- lexical-binding: t; -*-
;;;
(require 'whisper nil t)
;; does NOT throw an error if not found
;; since we don't really depend on whisper being installed, that's okay.

(after! whisper
  (setq
   whisper-install-whispercpp 'manual
   whisper-install-directory (expand-file-name "~/code/misc/")
   ;; =whisper.cpp= must be found inside `whisper-install-directory'.

   whisper-model "distil-large-v3"
   whisper-language "en"
   whisper-translate nil
   ;; whisper-use-threads (/ (num-processors) 2)

   whisper--ffmpeg-input-device ":0"
   ;; Use `rk/select-default-audio-device' to set this interactively.

   whisper-insert-text-at-point t
   )

  (defun night/whisper-post-process ()
    (let ((start (point-min))
          (end (point-max)))
      (copy-region-as-kill start end)))

  (add-hook 'whisper-post-process-hook #'night/whisper-post-process)

  (defun night/whisper-run-tmp ()
    ;; @broken
    ;; [[https://github.com/natrys/whisper.el/issues/27][Temporarily disabling `whisper-insert-text-at-point` · Issue #27 · natrys/whisper.el]]
    (interactive)
    (let ((whisper-insert-text-at-point nil))
      (whisper-run)))

  (comment
   ;; Use the system Whisper from Hammerspoon instead.
   (map!
    :nig
    "M-'" #'whisper-run
    ;; :leader
    ;; (:prefix ("v" . "voice")
    ;;  :desc "Transcribe last whisper" "w" #'whisper-run-buffer
    ;;  :desc "Transcribe file" "f" #'whisper-file
    ;;  :desc "Select audio device" "d" #'rk/select-default-audio-device)
    ))
;;;
  (defun rk/get-ffmpeg-device ()
    "Gets the list of devices available to ffmpeg.
The output of the ffmpeg command is pretty messy, e.g.
  [AVFoundation indev @ 0x7f867f004580] AVFoundation video devices:
  [AVFoundation indev @ 0x7f867f004580] [0] FaceTime HD Camera (Built-in)
  [AVFoundation indev @ 0x7f867f004580] AVFoundation audio devices:
  [AVFoundation indev @ 0x7f867f004580] [0] Cam Link 4K
  [AVFoundation indev @ 0x7f867f004580] [1] MacBook Pro Microphone
so we need to parse it to get the list of devices.
The return value contains two lists, one for video devices and one for audio devices.
Each list contains a list of cons cells, where the car is the device number and the cdr is the device name."
    (unless (string-equal system-type "darwin")
      (error "This function is currently only supported on macOS"))

    (let ((lines (string-split (shell-command-to-string "ffmpeg -list_devices true -f avfoundation -i dummy || true") "\n")))
      (cl-loop with at-video-devices = nil
               with at-audio-devices = nil
               with video-devices = nil
               with audio-devices = nil
               for line in lines
               when (string-match "AVFoundation video devices:" line)
               do (setq at-video-devices t
                        at-audio-devices nil)
               when (string-match "AVFoundation audio devices:" line)
               do (setq at-audio-devices t
                        at-video-devices nil)
               when (and at-video-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) video-devices)
               when (and at-audio-devices
                         (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
               do (push (cons (string-to-number (match-string 1 line)) (match-string 2 line)) audio-devices)
               finally return (list (nreverse video-devices) (nreverse audio-devices)))))

  (defun rk/find-device-matching (string type)
    "Get the devices from `rk/get-ffmpeg-device' and look for a device
matching `STRING'. `TYPE' can be :video or :audio."
    (let* ((devices (rk/get-ffmpeg-device))
           (device-list (if (eq type :video)
                            (car devices)
                          (cadr devices))))
      (cl-loop for device in device-list
               when (string-match-p string (cdr device))
               return (car device))))

  (defcustom rk/default-audio-device nil
    "The default audio device to use for whisper.el and outher audio processes."
    :type 'string)

  (defun rk/select-default-audio-device (&optional device-name)
    "Interactively select an audio device to use for whisper.el and other audio processes.
If `DEVICE-NAME' is provided, it will be used instead of prompting the user."
    (interactive)
    (let* ((audio-devices (cadr (rk/get-ffmpeg-device)))
           (indexes (mapcar #'car audio-devices))
           (names (mapcar #'cdr audio-devices))
           (name (or device-name (completing-read "Select audio device: " names nil t))))
      (setq rk/default-audio-device (rk/find-device-matching name :audio))
      (when (boundp 'whisper--ffmpeg-input-device)
        (setq whisper--ffmpeg-input-device (format ":%s" rk/default-audio-device)))))
;;;
  )
;;;
(provide 'night-whisper)
;;; night-whisper.el ends here

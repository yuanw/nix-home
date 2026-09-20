;;; whisper-device.el --- Interactively select FFmpeg audio device for whisper.el  -*- lexical-binding: t; -*-
;;
;; Use M-x whisper-select-audio-device to pick your mic.
;; Sets `whisper--ffmpeg-input-device' to the selected device (e.g. ":1").
;;
;; Based on https://github.com/natrys/whisper.el/wiki/MacOS-Configuration

(defun whisper--get-ffmpeg-devices ()
  "Parse ffmpeg avfoundation device list, return list of (index . name) for audio devices."
  (unless (eq system-type 'darwin)
    (error "Only supported on macOS"))
  (let* ((raw (shell-command-to-string
               "ffmpeg -f avfoundation -list_devices true -i dummy 2>&1"))
         (lines (split-string raw "\n"))
         audio-devices
         in-audio)
    (dolist (line lines)
      (when (string-match "AVFoundation audio devices:" line)
        (setq in-audio t))
      (when (and in-audio
                 (string-match "\\[\\([0-9]+\\)\\] \\(.+\\)" line))
        (push (cons (string-to-number (match-string 1 line))
                     (match-string 2 line))
              audio-devices)))
    (nreverse audio-devices)))

;;;###autoload
(defun whisper-select-audio-device ()
  "Interactively select an audio input device for whisper.el recording."
  (interactive)
  (let* ((devices (whisper--get-ffmpeg-devices))
         (names (mapcar #'cdr devices))
         (name (completing-read "Select audio device: " names nil t))
         (idx (cl-loop for (i . nm) in devices
                        when (equal nm name) return i)))
    (setq whisper--ffmpeg-input-device (format ":%d" idx))
    (message "Set whisper--ffmpeg-input-device to %s" whisper--ffmpeg-input-device)))

(provide 'whisper-device)
;;; whisper-device.el ends here
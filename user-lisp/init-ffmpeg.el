;;; init-ffmpeg.el --- init for ffmpeg               -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Keywords: multimedia, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Text-based FFmpeg User Interface.

;;; Code:

(defvar savehist-additional-variables)

(defcustom zr-ffmpeg-program "ffmpeg"
  "FFmpeg executable."
  :group 'zr
  :type 'file)

(defcustom zr-ffmpeg-live-framerate 30
  "Screen capture framerate."
  :group 'zr
  :type 'integer)

(defcustom zr-ffmpeg-live-video-codec "h264_nvenc"
  "Video codec."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-live-preset "p4"
  "Video encoder preset."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-live-audio-codec "aac"
  "Audio codec."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-live-audio-bitrate "128k"
  "Audio bitrate."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-live-capture-system-audio t
  "Whether to capture audio."
  :group 'zr
  :type 'boolean)

(defcustom zr-ffmpeg-live-audio-device
  "Stereo Mix (Realtek(R) Audio)"
  "Default DirectShow audio device."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-rtsp-transport "tcp"
  "RTSP transport."
  :group 'zr
  :type '(choice
          (const :tag "TCP" "tcp")
          (const :tag "UDP" "udp")))

(defcustom zr-ffmpeg-live-extra-args '("-nostats")
  "Additional FFmpeg arguments."
  :group 'zr
  :type '(repeat string))

(defvar zr-ffmpeg-publish-history nil)

(with-eval-after-load 'savehist
  (add-to-list
   'savehist-additional-variables
   'zr-ffmpeg-publish-history))

(defun zr-ffmpeg-system-processes ()
  "Return running system process executable names."
  (sort
   (delete-dups
    (delq nil
          (mapcar
           (lambda (pid)
             (cdr
              (assq 'comm
                    (process-attributes pid))))
           (list-system-processes))))
   #'string-lessp))

(defun zr-ffmpeg-dshow-audio-devices ()
  "Return available DirectShow audio devices."
  (let ((output
         (with-temp-buffer
           ;; FFmpeg writes the device list to stderr.
           (call-process
            zr-ffmpeg-program
            nil
            (list t t)
            nil
            "-hide_banner"
            "-list_devices" "true"
            "-f" "dshow"
            "-i" "dummy")
           (buffer-string))))
    (let (devices)
      (dolist (line (split-string output "\n" t))
        ;; Example:
        ;;
        ;;   "Microphone (Realtek(R) Audio)" (audio)
        ;;
        (when (string-match
               "\"\\([^\"]+\\)\"[ \t]*(audio)"
               line)
          (push (match-string 1 line)
                devices)))
      (delete-dups
       (nreverse devices)))))

(defun zr-ffmpeg--read-window-exe ()
  "Select a running Windows executable."
  (let ((processes (zr-ffmpeg-system-processes)))
    (unless processes
      (user-error "No system processes found"))
    (completing-read
     "Window executable: "
     processes
     nil
     t)))

(defun zr-ffmpeg--read-audio-device ()
  "Select a DirectShow audio device."
  (let ((devices (zr-ffmpeg-dshow-audio-devices)))
    (unless devices
      (user-error "No DirectShow audio devices found"))
    (let ((default
           (cond
            ((member zr-ffmpeg-live-audio-device devices)
             zr-ffmpeg-live-audio-device)
            ((member "Stereo Mix (Realtek(R) Audio)" devices)
             "Stereo Mix (Realtek(R) Audio)")
            (t
             (car devices)))))
      (completing-read
       "Audio device: "
       devices
       nil
       t
       nil
       nil
       default))))

(defun zr-ffmpeg--make-gfxcapture-filter (window-exe)
  "Return gfxcapture filter string for WINDOW-EXE."
  (format
   "gfxcapture=window_exe='%s':max_framerate=%d,hwdownload,format=bgra"
   (regexp-quote window-exe)
   zr-ffmpeg-live-framerate))

(defun zr-ffmpeg--build-publish-args
    (window-exe audio-device rtsp)
  "Build FFmpeg arguments for WINDOW-EXE, AUDIO-DEVICE and RTSP."
  (let ((video-filter
         (zr-ffmpeg--make-gfxcapture-filter window-exe)))
    (append
     ;; Video input.
     (list
      "-f" "lavfi"
      "-i" video-filter)

     ;; Audio input.
     (when audio-device
       (list
        "-f" "dshow"
        "-i" (format "audio=%s" audio-device)))

     ;; Explicit stream mapping.
     (list
      "-map" "0:v:0")

     (when audio-device
       (list
        "-map" "1:a:0"))

     ;; Video encoding.
     (list
      "-c:v" zr-ffmpeg-live-video-codec
      "-preset" zr-ffmpeg-live-preset
      "-pix_fmt" "yuv420p")

     ;; Audio encoding.
     (when audio-device
       (list
        "-c:a" zr-ffmpeg-live-audio-codec
        "-b:a" zr-ffmpeg-live-audio-bitrate))

     ;; RTSP output.
     (list
      "-f" "rtsp"
      "-rtsp_transport" zr-ffmpeg-rtsp-transport
      rtsp)

     ;; User supplied arguments.
     zr-ffmpeg-live-extra-args)))

;;;###autoload
(defun zr-ffmpeg-publish-rtsp-window ()
  "Capture a Windows window and publish it to MediaMTX via RTSP."
  (interactive)

  (let* ((window-exe
          (zr-ffmpeg--read-window-exe))

         ;; Ask for audio only when enabled.
         (audio-device
          (when zr-ffmpeg-live-capture-system-audio
            (zr-ffmpeg--read-audio-device)))

         (rtsp
          (read-string
           "RTSP URL: "
           nil
           'zr-ffmpeg-publish-history
           (car zr-ffmpeg-publish-history)))

         (args
          (zr-ffmpeg--build-publish-args
           window-exe
           audio-device
           rtsp))

         (old-process
          (get-process "ffmpeg-publish"))

         (buffer-name
          "*ffmpeg-publish*"))

    ;; Stop an existing publisher.
    (when (process-live-p old-process)
      (delete-process old-process))

    ;; Start FFmpeg asynchronously.
    (let ((process
           (make-process
            :name "ffmpeg-publish"
            :buffer buffer-name
            :command (cons zr-ffmpeg-program args)
            :connection-type 'pipe
            :noquery t)))

      ;; Save selected RTSP URL.
      (add-to-history
       'zr-ffmpeg-publish-history
       rtsp
       20)

      ;; Make it clear what was started.
      (message
       "FFmpeg publishing %s%s to %s"
       window-exe
       (if audio-device
           (format " with audio=%s" audio-device)
         "")
       rtsp)

      process)))

;;;###autoload
(defun zr-ffmpeg-stop-publish ()
  "Stop the current FFmpeg publishing process."
  (interactive)

  (let ((process
         (get-process "ffmpeg-publish")))

    (if (process-live-p process)
        (progn
          (delete-process process)
          (message "FFmpeg publishing stopped"))
      (user-error
       "FFmpeg publishing process is not running"))))

(provide 'init-ffmpeg)
;;; init-ffmpeg.el ends here

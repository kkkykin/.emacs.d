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

(require 'cl-lib)
(require 'dired)
(require 'subr-x)
(require 'transient)

(defgroup zr-ffmpeg nil
  "FFmpeg integration."
  :group 'multimedia)

(defcustom zr-ffmpeg-program "ffmpeg"
  "FFmpeg executable."
  :type 'file)

(defcustom zr-ffmpeg-log-level "warning"
  "FFmpeg log level."
  :type 'string)

(defcustom zr-ffmpeg-window-hwnd nil
  "HWND used by the screen capture preset.

Can be a string such as \"0x123456\".

This is intentionally separate from the preset because the target window
usually changes independently from the streaming configuration."
  :type '(choice (const nil) string))

(defcustom zr-ffmpeg-window-exe nil
  "Executable name used by the screen capture preset.

For example:

  \"StreetsOfRogue.exe\"

Normally `zr-ffmpeg-window-hwnd' is preferred when available."
  :type '(choice (const nil) string))

(defun zr-ffmpeg-system-processes ()
  "Return running system process executable names."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (pid)
            (cdr
             (assq 'comm
                   (process-attributes pid))))
          (list-system-processes)))))

(defcustom zr-ffmpeg-window-framerate 30
  "Default capture framerate for screen capture."
  :type 'integer)

(defcustom zr-ffmpeg-audio-device nil
  "Default DirectShow audio device.

For example:

  \"Stereo Mix (Realtek(R) Audio)\""
  :type '(choice (const nil) string))

(defun zr-ffmpeg-dshow-audio-devices ()
  "Return available DirectShow audio devices."
  (let ((output
         (with-temp-buffer
           (call-process zr-ffmpeg-program nil (list t t) nil
                         "-hide_banner"
                         "-list_devices" "true"
                         "-f" "dshow"
                         "-i" "dummy")
           (buffer-string))))
    (let (devices)
      (dolist (line (split-string output "\n" t))
        (when (string-match "\"\\([^\"]+\\)\"[ \t]*(audio)" line)
          (push (match-string 1 line) devices)))
      (delete-dups (nreverse devices)))))

(defcustom zr-ffmpeg-stream-target nil
  "Default streaming target."
  :type 'string)

(defcustom zr-ffmpeg-output-directory nil
  "Optional default directory for file output.

When nil, output files are written next to the input files."
  :type '(choice (const nil) directory))

(defcustom zr-ffmpeg-extra-args '("-nostats")
  "Additional FFmpeg arguments appended to every invocation.

This is intended for uncommon options which are not exposed by the
transient menu."
  :type '(repeat string))


;;; State

(defvar-local zr-ffmpeg--preset 'file-stream
  "Currently selected FFmpeg preset.")

(defvar-local zr-ffmpeg--input nil
  "Input override.

Possible values:

  nil       use preset
  file
  screen")

(defvar-local zr-ffmpeg--video-codec nil)
(defvar-local zr-ffmpeg--video-preset nil)
(defvar-local zr-ffmpeg--video-bitrate nil)
(defvar-local zr-ffmpeg--framerate nil)
(defvar-local zr-ffmpeg--fps-mode nil)

(defvar-local zr-ffmpeg--audio nil)
(defvar-local zr-ffmpeg--audio-codec nil)
(defvar-local zr-ffmpeg--audio-bitrate nil)

(defvar-local zr-ffmpeg--output-format nil)
(defvar-local zr-ffmpeg--output-target nil)

(defvar-local zr-ffmpeg--extra-args nil)


;;; Presets

(defcustom zr-ffmpeg-presets
  '((screen-stream
     :description "Screen → Stream"
     :input screen
     :mode stream
     :video-codec "h264_nvenc"
     :video-preset "p4"
     :video-bitrate "3M"
     :framerate 30
     :fps-mode "cfr"
     :audio t
     :audio-codec "libopus"
     :audio-bitrate "128k"
     :video-args ("-pix_fmt" "yuv420p"
                  "-ts_buffer_size" "2M"
                  "-bf" "0")
     :output-format "whip")

    (file-stream
     :description "File → Stream"
     :input file
     :mode stream
     :video-codec "h264_nvenc"
     :video-preset "p4"
     :video-bitrate "2M"
     :fps-mode "cfr"
     :audio t
     :audio-codec "libopus"
     :video-args ("-pix_fmt" "yuv420p"
                  "-ts_buffer_size" "2M"
                  "-bf" "0")
     :output-format "whip")

    (file-copy
     :description "File → File"
     :input file
     :mode file
     :video-codec "copy"
     :audio-codec "copy"
     :output-format "mp4"
     :movflags "+faststart")

    (screen-record
     :description "Screen → File"
     :input screen
     :mode file
     :video-codec "hevc_nvenc"
     :video-preset "p4"
     :video-bitrate "6000k"
     :framerate 30
     :fps-mode "vbr"
     :audio t
     :audio-codec "libopus"
     :audio-bitrate "128k"
     :output-format "mp4"
     :movflags "+faststart"))
  "FFmpeg presets.

Each element has the form:

  (NAME
   :description STRING
   :input SYMBOL
   :mode SYMBOL
   ...)

Preset values provide defaults only.  Values explicitly changed in the
transient override them."
  :type '(alist :key-type symbol :value-type plist))

(defun zr-ffmpeg--preset ()
  "Return the currently selected preset plist."
  (or (cdr (assq zr-ffmpeg--preset zr-ffmpeg-presets))
      (user-error "Unknown FFmpeg preset: %s" zr-ffmpeg--preset)))

(defun zr-ffmpeg--preset-value (key)
  "Return KEY from the current preset."
  (plist-get (zr-ffmpeg--preset) key))

(defun zr-ffmpeg--value (variable key)
  "Return VARIABLE's override or preset KEY."
  (or (and variable (symbol-value variable))
      (zr-ffmpeg--preset-value key)))

(defun zr-ffmpeg--preset-name ()
  "Return a human-readable name of the current preset."
  (or (plist-get (zr-ffmpeg--preset) :description)
      (symbol-name zr-ffmpeg--preset)))

(defun zr-ffmpeg--preset-candidates ()
  "Return completion candidates for presets."
  (mapcar
   (lambda (entry)
     (cons
      (symbol-name (car entry))
      (or (plist-get (cdr entry) :description)
          (symbol-name (car entry)))))
   zr-ffmpeg-presets))

(defun zr-ffmpeg--read-preset (prompt _initial-input _history)
  "Read an FFmpeg preset."
  (intern
   (completing-read prompt (zr-ffmpeg--preset-candidates) nil t nil nil
                    (symbol-name zr-ffmpeg--preset))))

(defun zr-ffmpeg--select-preset ()
  "Select an FFmpeg preset."
  (interactive)
  (setq zr-ffmpeg--preset (zr-ffmpeg--read-preset "Preset: " nil nil))
  (transient-setup))

(defun zr-ffmpeg--reset-overrides ()
  "Clear all transient overrides."
  (interactive)
  (setq zr-ffmpeg--input nil
        zr-ffmpeg--video-codec nil
        zr-ffmpeg--video-preset nil
        zr-ffmpeg--video-bitrate nil
        zr-ffmpeg--framerate nil
        zr-ffmpeg--fps-mode nil
        zr-ffmpeg--audio nil
        zr-ffmpeg--audio-codec nil
        zr-ffmpeg--audio-bitrate nil
        zr-ffmpeg--output-format nil
        zr-ffmpeg--output-target nil
        zr-ffmpeg--extra-args nil)
  (transient-setup))


;;; Input

(defun zr-ffmpeg--input-type ()
  "Return effective input type."
  (or zr-ffmpeg--input
      (zr-ffmpeg--preset-value :input)
      (if (derived-mode-p 'dired-mode)
          'file
        'file)))

(defun zr-ffmpeg--selected-files ()
  "Return files selected by the current context."
  (or (dired-get-marked-files)
      (list (buffer-file-name))))


;;; Screen capture

(defun zr-ffmpeg--screen-filter ()
  "Build the screen capture filter."
  (cond
   (zr-ffmpeg-window-hwnd
    (format
     "gfxcapture=window_hwnd=%s:max_framerate=%s,hwdownload,format=bgra"
     zr-ffmpeg-window-hwnd
     (or (zr-ffmpeg--value
          'zr-ffmpeg--framerate
          :framerate)
         zr-ffmpeg-window-framerate)))

   (zr-ffmpeg-window-exe
    (format
     "gfxcapture=window_exe='%s':max_framerate=%s,hwdownload,format=bgra"
     zr-ffmpeg-window-exe
     (or (zr-ffmpeg--value
          'zr-ffmpeg--framerate
          :framerate)
         zr-ffmpeg-window-framerate)))

   (t
    (user-error
     "Set `zr-ffmpeg-window-hwnd' or `zr-ffmpeg-window-exe' first"))))

(defun zr-ffmpeg--screen-input-args ()
  "Return FFmpeg arguments for screen capture."
  (list "-f" "lavfi" "-i" (zr-ffmpeg--screen-filter)))

(defun zr-ffmpeg--audio-input-args ()
  "Return FFmpeg arguments for DirectShow audio."
  (when-let* ((device zr-ffmpeg-audio-device))
    (list "-f" "dshow" "-i" (format "audio=%s" device))))


;;; Video / audio / output arguments

(defun zr-ffmpeg--video-args ()
  "Build video processing arguments."
  (let ((codec (zr-ffmpeg--value 'zr-ffmpeg--video-codec :video-codec))
        (preset (zr-ffmpeg--value 'zr-ffmpeg--video-preset :video-preset))
        (bitrate (zr-ffmpeg--value 'zr-ffmpeg--video-bitrate :video-bitrate))
        (framerate (zr-ffmpeg--value 'zr-ffmpeg--framerate :framerate))
        (fps-mode (zr-ffmpeg--value 'zr-ffmpeg--fps-mode :fps-mode))
        (video-args (zr-ffmpeg--value nil :video-args)))
    (append
     (when codec (list "-c:v" codec))
     (when preset (list "-preset" preset))
     (when bitrate (list "-b:v" bitrate))

     ;; Keep live capture FPS separate from file remux/copy use-cases.
     (when (and framerate (eq (zr-ffmpeg--input-type) 'screen))
       (list "-r" (format "%s" framerate)))

     (when fps-mode (list "-fps_mode" fps-mode))
     video-args)))

(defun zr-ffmpeg--audio-enabled-p ()
  "Whether audio should be enabled."
  (let ((override zr-ffmpeg--audio))
    (if (null override)
        (zr-ffmpeg--preset-value :audio)
      override)))

(defun zr-ffmpeg--audio-args ()
  "Build audio processing arguments."
  (when (zr-ffmpeg--audio-enabled-p)
    (let ((codec (zr-ffmpeg--value
                  'zr-ffmpeg--audio-codec
                  :audio-codec))
          (bitrate (zr-ffmpeg--value
                    'zr-ffmpeg--audio-bitrate
                    :audio-bitrate))
          (audio-args (zr-ffmpeg--value nil :audio-args)))
      (append
       (when codec (list "-c:a" codec))
       (when bitrate (list "-b:a" bitrate))
       audio-args))))

(defun zr-ffmpeg--output-args ()
  "Build common output arguments."
  (append
   (list "-loglevel" zr-ffmpeg-log-level)

   (when (eq (zr-ffmpeg--preset-value :mode) 'file)
     (list "-movflags"
           (or (zr-ffmpeg--preset-value :movflags)
               "+faststart")))

   (when-let* ((format
                (zr-ffmpeg--value
                 'zr-ffmpeg--output-format
                 :output-format)))
     (list "-f" format))

   zr-ffmpeg-extra-args
   zr-ffmpeg--extra-args))


;;; Output target

(defun zr-ffmpeg--stream-target ()
  "Return effective streaming target."
  (or zr-ffmpeg--output-target
      zr-ffmpeg-stream-target))

(defun zr-ffmpeg--file-output-path (input)
  "Return output path corresponding to INPUT."
  (let* ((base (file-name-sans-extension
                (file-name-nondirectory input)))
         (extension
          (pcase (zr-ffmpeg--value
                  nil
                  :output-format)
            ("matroska" ".mkv")
            ("mp4" ".mp4")
            ("mpegts" ".ts")
            (_ ".mkv")))
         (directory
          (or zr-ffmpeg-output-directory
              (file-name-directory input))))
    (expand-file-name
     (concat base extension)
     directory)))

(defun zr-ffmpeg--output-for-input (input)
  "Return output target for INPUT."
  (if (eq (zr-ffmpeg--preset-value :mode) 'stream)
      (zr-ffmpeg--stream-target)
    (zr-ffmpeg--file-output-path input)))


;;; Command construction

(defun zr-ffmpeg--input-args (file)
  "Build input arguments for FILE."
  (pcase (zr-ffmpeg--input-type)
    ('screen
     (zr-ffmpeg--screen-input-args))
    ('file (unless file (user-error "No input file")) (list "-i" file))
    (_ (user-error "Unsupported input type: %s" (zr-ffmpeg--input-type)))))

(defun zr-ffmpeg--mapping-args ()
  "Return stream mapping arguments."
  (if (eq (zr-ffmpeg--input-type) 'screen)
      (append
       (list "-map" "0:v:0")
       (when (zr-ffmpeg--audio-enabled-p)
         (list "-map" "1:a:0?")))
    (append
     (list "-map" "0:v:0")
     (when (zr-ffmpeg--audio-enabled-p)
       (list "-map" "0:a:0?")))))

(defun zr-ffmpeg--command (file output)
  "Build FFmpeg argv for FILE and OUTPUT."
  (append
   (list zr-ffmpeg-program)

   ;; Screen capture has video on input #0.
   (zr-ffmpeg--input-args file)

   ;; Screen + DirectShow audio is a second input.
   (when (and (eq (zr-ffmpeg--input-type) 'screen)
              (zr-ffmpeg--audio-enabled-p))
     (zr-ffmpeg--audio-input-args))

   (zr-ffmpeg--mapping-args)

   (zr-ffmpeg--video-args)
   (zr-ffmpeg--audio-args)

   (when (eq (zr-ffmpeg--preset-value :mode) 'stream)
     ;; Streaming should avoid unnecessary buffering.
     (list
      "-fflags" "+nobuffer"
      "-flags" "+low_delay"))

   (zr-ffmpeg--output-args)

   ;; FFmpeg needs the target last.
   (list output)))


;;; Formatting

(defun zr-ffmpeg--shell-quote (arg)
  "Quote ARG as a shell argument."
  (shell-quote-argument arg))

(defun zr-ffmpeg--command-string (args)
  "Return shell representation of ARGS."
  (mapconcat #'zr-ffmpeg--shell-quote args " "))

(defun zr-ffmpeg--preview-command ()
  "Return the command that would be executed."
  (let* ((type (zr-ffmpeg--input-type))
         (files (zr-ffmpeg--selected-files))
         (file (car files)))
    (when (and (eq type 'file)
               (null file))
      (user-error "No input file"))

    (zr-ffmpeg--command-string
     (zr-ffmpeg--command
      file
      (zr-ffmpeg--output-for-input file)))))


;;; Run

(defun zr-ffmpeg--start-command (args)
  "Start FFmpeg with ARGS."
  (let ((buffer (get-buffer-create "*zr-ffmpeg*")))
    (message "%s" (zr-ffmpeg--command-string args))
    (apply #'start-process "zr-ffmpeg" buffer args)))

(defun zr-ffmpeg--run-screen ()
  "Run a screen capture preset."
  (let ((target (zr-ffmpeg--stream-target)))
    (zr-ffmpeg--start-command (zr-ffmpeg--command nil target))))

(defun zr-ffmpeg--run-file (file)
  "Run FFmpeg on FILE."
  (let ((output (zr-ffmpeg--output-for-input file)))
    ;; Avoid accidentally using the same path.
    (when (equal (file-truename file)
                 (file-truename output))
      (user-error "Input and output are identical: %s" file))

    (zr-ffmpeg--start-command (zr-ffmpeg--command file output))))

(defun zr-ffmpeg-run ()
  "Run FFmpeg according to the transient state."
  (interactive)
  (pcase (zr-ffmpeg--input-type)
    ('screen
     (zr-ffmpeg--run-screen))

    ('file
     (let ((files (zr-ffmpeg--selected-files))
           (mode (zr-ffmpeg--preset-value :mode)))
       (unless files
         (user-error "No input file"))

       ;; A streaming endpoint cannot reasonably have several independent
       ;; FFmpeg processes writing to it.
       (when (and (eq mode 'stream)
                  (> (length files) 1))
         (user-error
          "Stream preset accepts one input; %d files are selected"
          (length files)))

       (dolist (file files)
         (zr-ffmpeg--run-file file))))

    (_
     (user-error "Unsupported input type"))))

(defun zr-ffmpeg-preview ()
  "Preview the FFmpeg command."
  (interactive)
  (message "%s" (zr-ffmpeg--preview-command)))


;;; Transient readers

(defun zr-ffmpeg--read-string (prompt _initial-input _history)
  "Read a string."
  (read-string prompt))

(defun zr-ffmpeg--read-number (prompt _initial-input history)
  "Read a number."
  (let ((default
         (and (symbolp (car-safe history))
              (symbol-value (car history)))))
    (read-number prompt default)))

(defun zr-ffmpeg--read-video-codec
    (prompt _initial-input _history)
  "Read a video codec."
  (completing-read
   prompt
   '("copy" "h264_nvenc" "hevc_nvenc" "libx264" "libx265")
   nil t))

(defun zr-ffmpeg--read-audio-codec
    (prompt _initial-input _history)
  "Read an audio codec."
  (completing-read prompt '("copy" "aac" "libopus" "pcm_s16le") nil t))

(defun zr-ffmpeg--read-fps-mode
    (prompt _initial-input _history)
  "Read FPS mode."
  (completing-read prompt '("auto" "passthrough" "cfr" "vfr") nil t))

(defun zr-ffmpeg--read-input
    (prompt _initial-input _history)
  "Read input type."
  (intern
   (completing-read
    prompt
    '(("file" . "File / Dired")
      ("screen" . "Screen"))
    nil
    t)))


;;; Transient infixes

(transient-define-infix zr-ffmpeg-preset-infix ()
  :class 'transient-lisp-variable
  :key "p"
  :description "Preset"
  :variable 'zr-ffmpeg--preset
  :reader #'zr-ffmpeg--read-preset)

(transient-define-infix zr-ffmpeg-input-infix ()
  :class 'transient-lisp-variable
  :key "i"
  :description "Input"
  :variable 'zr-ffmpeg--input
  :reader #'zr-ffmpeg--read-input)

(transient-define-infix zr-ffmpeg-video-codec-infix ()
  :class 'transient-lisp-variable
  :key "v"
  :description "Codec"
  :variable 'zr-ffmpeg--video-codec
  :reader #'zr-ffmpeg--read-video-codec)

(transient-define-infix zr-ffmpeg-video-preset-infix ()
  :class 'transient-lisp-variable
  :key "P"
  :description "Preset"
  :variable 'zr-ffmpeg--video-preset
  :reader #'zr-ffmpeg--read-string)

(transient-define-infix zr-ffmpeg-video-bitrate-infix ()
  :class 'transient-lisp-variable
  :key "b"
  :description "Bitrate"
  :variable 'zr-ffmpeg--video-bitrate
  :reader #'zr-ffmpeg--read-string)

(transient-define-infix zr-ffmpeg-framerate-infix ()
  :class 'transient-lisp-variable
  :key "r"
  :description "Framerate"
  :variable 'zr-ffmpeg--framerate
  :reader #'zr-ffmpeg--read-number)

(transient-define-infix zr-ffmpeg-fps-mode-infix ()
  :class 'transient-lisp-variable
  :key "m"
  :description "FPS mode"
  :variable 'zr-ffmpeg--fps-mode
  :reader #'zr-ffmpeg--read-fps-mode)

(transient-define-infix zr-ffmpeg-audio-infix ()
  :class 'transient-lisp-variable
  :key "t"
  :description "Audio"
  :variable 'zr-ffmpeg--audio
  :reader
  (lambda (_prompt _initial-input _history)
    (not zr-ffmpeg--audio)))

(transient-define-infix zr-ffmpeg-audio-codec-infix ()
  :class 'transient-lisp-variable
  :key "a"
  :description "Codec"
  :variable 'zr-ffmpeg--audio-codec
  :reader #'zr-ffmpeg--read-audio-codec)

(transient-define-infix zr-ffmpeg-audio-bitrate-infix ()
  :class 'transient-lisp-variable
  :key "B"
  :description "Bitrate"
  :variable 'zr-ffmpeg--audio-bitrate
  :reader #'zr-ffmpeg--read-string)

(transient-define-infix zr-ffmpeg-output-format-infix ()
  :class 'transient-lisp-variable
  :key "f"
  :description "Format"
  :variable 'zr-ffmpeg--output-format
  :reader
  (lambda (prompt _initial-input _history)
    (completing-read
     prompt
     '("rtsp"
       "rtmp"
       "srt"
       "mpegts"
       "matroska"
       "mp4")
     nil
     t)))

(transient-define-infix zr-ffmpeg-output-target-infix ()
  :class 'transient-lisp-variable
  :key "o"
  :description "Target"
  :variable 'zr-ffmpeg--output-target
  :reader #'zr-ffmpeg--read-string)


;;; Main transient

(transient-define-prefix zr-ffmpeg-menu ()
  "FFmpeg transient."
  :refresh-suffixes t

  [["Preset"
    (zr-ffmpeg-preset-infix)
    ("R" "Reset overrides"
     zr-ffmpeg--reset-overrides
     :transient t)]

   ["Input"
    (zr-ffmpeg-input-infix)
    ("W" "Set window"
     (lambda ()
       (interactive)
       (setq zr-ffmpeg-window-exe
             (completing-read
              "Window: "
              (zr-ffmpeg-system-processes)))
       (transient-setup))
     :transient t)
    ("A" "Set audio device"
     (lambda ()
       (interactive)
       (setq zr-ffmpeg-audio-device
             (completing-read
              "Audio device: "
              (zr-ffmpeg-dshow-audio-devices)))
       (transient-setup))
     :transient t)]

   ["Video"
    (zr-ffmpeg-video-codec-infix)
    (zr-ffmpeg-video-preset-infix)
    (zr-ffmpeg-video-bitrate-infix)
    (zr-ffmpeg-framerate-infix)
    (zr-ffmpeg-fps-mode-infix)]

   ["Audio"
    (zr-ffmpeg-audio-infix)
    (zr-ffmpeg-audio-codec-infix)
    (zr-ffmpeg-audio-bitrate-infix)]

   ["Output"
    (zr-ffmpeg-output-format-infix)
    (zr-ffmpeg-output-target-infix)]

   ["Run"
    ("RET" "Run"
     zr-ffmpeg-run
     :transient nil)

    ("SPC" "Preview"
     zr-ffmpeg-preview
     :transient t)

    ("e" "Extra args"
     (lambda ()
       (interactive)
       (setq zr-ffmpeg--extra-args
             (split-string-and-unquote
              (read-string
               "Extra FFmpeg args: "
               (string-join zr-ffmpeg--extra-args " "))))
       (transient-setup))
     :transient t)]])

;;;###autoload
(defun zr-ffmpeg ()
  "Open FFmpeg transient."
  (interactive)
  (zr-ffmpeg-menu))

(provide 'zr-ffmpeg)

;;; zr-ffmpeg.el ends here

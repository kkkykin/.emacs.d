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
(defvar zr-dotfiles-dir)

(defcustom zr-ffmpeg-program "ffmpeg"
  "FFmpeg executable."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-window-script
  (expand-file-name "uv/_tangle/scripts/win-windows.py" zr-dotfiles-dir)
  "Python script used to enumerate Windows."
  :group 'zr
  :type 'file)

(defcustom zr-ffmpeg-live-framerate 30
  "Screen capture framerate."
  :group 'zr
  :type 'integer)

(defcustom zr-ffmpeg-live-video-codec "libx264"
  "Video codec."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-live-preset "veryfast"
  "x264 preset."
  :group 'zr
  :type 'string)

(defcustom zr-ffmpeg-rtsp-transport "tcp"
  "RTSP transport."
  :group 'zr
  :type '(choice
          (const "tcp")
          (const "udp")))

(defcustom zr-ffmpeg-live-extra-args '("-nostats")
  "Additional FFmpeg arguments."
  :group 'zr
  :type '(repeat string))

(defun zr-ffmpeg-windows ()
  "Return available Windows windows as (TITLE . HWND)."
  (let ((output
         (with-temp-buffer
           (call-process
            "uv" nil t nil
            "run" zr-ffmpeg-window-script)
           (buffer-string))))
    (mapcar
     (lambda (line)
       (pcase-let ((`(,hwnd ,title)
                    (split-string line "\t" t)))
         (cons title hwnd)))
     (split-string output "\n" t))))

(defvar zr-ffmpeg-publish-history nil)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'zr-ffmpeg-publish-history))

;;;###autoload
(defun zr-ffmpeg-publish-rtsp-window ()
  "Capture a Windows window with FFmpeg and publish it to MediaMTX."
  (interactive)
  (let* ((windows (zr-ffmpeg-windows))
         (title (completing-read "Window: " (mapcar #'car windows)))
         (rtsp (read-string "rtsp: " nil
                            'zr-ffmpeg-publish-history
                            (car zr-ffmpeg-publish-history)))
         (args
          (append
           (list
            "-f" "gdigrab"
            "-framerate" (number-to-string zr-ffmpeg-live-framerate)
            "-i" (format "title=%s" title)
            "-c:v" zr-ffmpeg-live-video-codec
            "-preset" zr-ffmpeg-live-preset
            "-f" "rtsp"
            "-rtsp_transport" zr-ffmpeg-rtsp-transport
            rtsp)
           zr-ffmpeg-live-extra-args))
         (process
          (apply #'start-process
                 "ffmpeg-publish"
                 "*ffmpeg-publish*"
                 zr-ffmpeg-program
                 args)))
    (add-to-history 'zr-ffmpeg-publish-history rtsp 5)
    (message "FFmpeg publishing %s" title)
    process))

(provide 'init-ffmpeg)
;;; init-ffmpeg.el ends here

;;; init-emms.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  kkky

;; Author: kkky <kkky@asus>
;; Keywords: multimedia

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

;; 

;;; Code:

(require 'emms-info)
(require 'emms-tag-editor)

(defvar zr-emms-info-ffprobe-field-map
  '((info-album       . ("album"))
    (info-artist      . ("artist" "author"))
    (info-title       . ("title"))
    (info-tracknumber . ("track" "tracknumber"))
    (info-composer    . ("composer"))
    (info-year        . ("date" "year"))
    (info-discnumber  . ("disc" "discnumber"))
    (info-genre       . ("genre"))
    (info-note        . ("comment" "description"))
    (info-playing-time . duration)  ; special handling
    (info-albumartist . ("album_artist" "albumartist")))
  "Mapping for ffprobe metadata fields.

Each element is a cons cell (EMMS-FIELD . FFMPEG-FIELD).
FFMPEG-FIELD is either a list of strings (attempted in order) for
metadata tags, or the symbol `duration' for the playing time.")

(defun zr-emms-info-ffprobe-duration (json-data)
  "Extract duration from ffprobe JSON-DATA and return as integer seconds."
  (let ((duration-str
         ;; Try format duration first
         (or (gethash "duration" (gethash "format" json-data))
             ;; Fallback to audio stream duration
             (catch 'found
               (dolist (stream (gethash "streams" json-data))
                 (when (equal (gethash "codec_type" stream) "audio")
                   (when-let* ((dur (gethash "duration" stream)))
                     (throw 'found dur)))))
             ;; Last resort: video stream duration
             (gethash "duration" (car (gethash "streams" json-data))))))
    (if duration-str
        (floor (string-to-number duration-str))
      0)))

(defun zr-emms-info-ffprobe-get-metadata (json-data field-names)
  "Extract metadata value from ffprobe JSON-DATA.
FIELD-NAMES is a list of strings to try in order.
Searches format.tags first, then each stream's tags."
  (catch 'found
    (dolist (name field-names)
      ;; Check format tags
      (when-let* ((format-tags (gethash "tags" (gethash "format" json-data))))
        (when-let* ((value (gethash name format-tags)))
          (throw 'found value)))
      ;; Check stream tags
      (mapc
       (lambda (stream)
         (when-let* ((stream-tags (gethash "tags" stream)))
           (when-let* ((value (gethash name stream-tags)))
             (throw 'found value))))
       (gethash "streams" json-data)))))

(defun zr-emms-info-ffprobe (track)
  "Set metadata for TRACK using ffprobe.
Supports multiple audio formats through ffmpeg's libavformat."
  (when (eq (emms-track-type track) 'file)
    (with-temp-buffer
      (when (zerop
             (let ((coding-system-for-read 'utf-8))
               (call-process "ffprobe" nil '(t nil) nil
                             "-v" "quiet"
                             "-print_format" "json"
                             "-show_format"
                             "-show_streams"
                             (emms-track-name track))))
        (goto-char (point-min))
        (condition-case nil
            (let ((json-data (json-parse-buffer)))
              (mapc
               (lambda (field-map)
                 (pcase-let ((emms-field (car field-map))
                             (ffprobe-field (cdr field-map)))
                   (cond
                    ;; Handle duration specially
                    ((eq ffprobe-field 'duration)
                     (let ((duration (zr-emms-info-ffprobe-duration json-data)))
                       (when (> duration 0)
                         (emms-track-set track 'info-playing-time duration))))
                    ;; Handle string metadata fields
                    ((listp ffprobe-field)
                     (when-let* ((value (zr-emms-info-ffprobe-get-metadata json-data ffprobe-field)))
                       (emms-track-set
                        track
                        emms-field
                        (if (eq emms-field 'info-year)
                            ;; Extract 4-digit year from date strings
                            (let ((date-str (format "%s" value)))
                              (if (string-match "\\`\\([0-9]\\{4\\}\\)" date-str)
                                  (match-string 1 date-str)
                                date-str))
                          ;; Other fields: ensure string format
                          (format "%s" value))))))))
               zr-emms-info-ffprobe-field-map))
          (error (message "Error reading metadata for %s with ffprobe"
                          (emms-track-name track))))
        track))))

(cond
 ((executable-find "exiftool")
  (require 'emms-info-exiftool)
  (setq emms-info-functions '(emms-info-exiftool)))
 ((executable-find "ffprobe")
  (setq emms-info-functions '(zr-emms-info-ffprobe))))

(defun zr-emms-tag-editor-tag-ffmpeg (track)
  "使用 ffmpeg 提交对 TRACK 的标签更改。
此函数会复制音频流并写入新元数据，然后用临时文件替换原文件。"
  (let* ((filename (emms-track-name track))
         ;; 将 EMMS 标签映射到 ffmpeg 元数据键
         (tags '(("artist" . info-artist)
                 ("album_artist" . info-albumartist)
                 ("composer" . info-composer)
                 ("performer" . info-performer)
                 ("title" . info-title)
                 ("album" . info-album)
                 ("track" . info-tracknumber)
                 ("disc" . info-discnumber) ; 光盘编号
                 ("date" . info-year)
                 ("genre" . info-genre)
                 ("comment" . info-note)))
         (extension (file-name-extension filename))
         (tempfile (if extension
                       (concat (make-temp-file "emms-ffmpeg-") "." extension)
                     (make-temp-file "emms-ffmpeg-")))
         (args `("-i" ,filename))
         val)
    ;; 构建 -metadata 参数列表
    (dolist (tag tags)
      (when (and (setq val (emms-track-get track (cdr tag)))
                 (stringp val)
                 (> (length val) 0))
        (setq args (append args `("-metadata" ,(concat (car tag) "=" val))))))
    ;; 如果没有元数据需要写入，直接返回
    (if (= (length args) 2)             ; 只有 "-i" 和文件名
        (emms-tag-editor-log "No metadata to write for %s" filename)
      ;; 添加输出参数并执行 ffmpeg
      (setq args (append args `("-codec" "copy" "-y" ,tempfile)))
      (let ((exit-code (apply #'call-process "ffmpeg" nil
                              (get-buffer-create emms-tag-editor-log-buffer) nil
                              args)))
        (when (zerop exit-code)
          (rename-file tempfile filename t))
        (ignore-errors (delete-file tempfile))
        exit-code))))

(setq emms-tag-editor-rename-format "%a/%l/%n - %t")
(unless (executable-find "mid3v2")
  (add-to-list 'emms-tag-editor-tagfile-functions
               '("mp3" . zr-emms-tag-editor-tag-ffmpeg)))
(add-to-list 'emms-tag-editor-tagfile-functions
             '("m4a" . zr-emms-tag-editor-tag-ffmpeg))

(provide 'init-emms)
;;; init-emms.el ends here

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

(transient-define-prefix my/ffmpeg-menu ()
  "ffmpeg Menu."
  ["Global Options"
   ("b" "Suppress printing banner." (nil "-hide_banner"))
   ("y" "Overwrite output files without asking." (nil "-y"))
   ("h" "Use hardware acceleration to decode the matching stream(s)."
    ("-hwaccel" "auto"))]

  ["Input Options"]

  ["Output Options"
   ("c"
    "Codex copy."
    ("-c" "copy"))
   ("s"
    "Embed subtitles to video. `-disposition:s:0 default' or `-c:s mov_text'"
    ("-disposition:s:0" "default"))])



(provide 'init-ffmpeg)
;;; init-ffmpeg.el ends here

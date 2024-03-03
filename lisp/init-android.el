;;; init-android.el --- android setup                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <kkky@KKSBOW>
;; Keywords: local

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

(defun my/mini-screen-setup-maybe ()
  "Setup for mini screen mobile."
  (if (= (display-pixel-height) 260)
      (modifier-bar-mode -1)
    (modifier-bar-mode 1)))

;; todo
(defun my/mpv-intent (scheme &optional sub)
  "http://mpv-android.github.io/mpv-android/intent.html
am: [-e|--es <EXTRA_KEY> <EXTRA_STRING_VALUE> ...]  filepath
scheme: rtmp, rtmps, rtp, rtsp, mms, mmst, mmsh, tcp, udp,
content, file, http, https example:
am start -n is.xyz.mpv/is.xyz.mpv.MPVActivity -e filepath
file:///sdcard/Music/local

optional: - decode_mode (Byte): if set to 2, hardware decoding
will be disabled - subs (ParcelableArray of Uri): list of
subtitle URIs to be added as additional tracks -
subs.enable (ParcelableArray of Uri): specifies which of the
subtitles should be selected by default, subset of previous array
- position (Int): starting point of video playback in
milliseconds"
  (when (file-exists-p scheme)
    (let ((url (concat "file://" scheme)))
      (start-process "" nil "am" "start" "-n"
                     "is.xyz.mpv/is.xyz.mpv.MPVActivity"
                     "-e" "filepath" url))))

(defun my/fooview-run (cmd)
  "Run fooview action."
  (start-process "fooview-run" nil "am" "start" "-a"
                 "com.fooview.android.intent.RUN_WORKFLOW" "-e" "action"
                 cmd "com.fooview.android.fooview/.ShortcutProxyActivity"))

(defun my/rish-run (cmd)
  "Run command with rish."
  (start-process "rish-run" nil "rish" "-c" cmd))

(defun my/normal-keyboard ()
  "Enable normal keyboard on android."
  (my/rish-run "ime set com.samsung.android.honeyboard/.service.HoneyBoardService"))

(defun my/bare-keyboard ()
  "Enable bare keyboard on android."
  (my/rish-run "ime set keepass2android.keepass2android/keepass2android.softkeyboard.KP2AKeyboard"))

(setenv "SSH_AUTH_SOCK" (string-trim-right (shell-command-to-string "gpgconf -L agent-ssh-socket")))

(let ((move-to-header '( mode-line-frame-identification
                         mode-line-buffer-identification "   "
                         (project-mode-line project-mode-line-format)
                         (vc-mode vc-mode) "  " mode-line-misc-info))
      (headers (if header-line-format header-line-format '("%e")))
      (modes (copy-sequence mode-line-format)))
  (dolist (item move-to-header)
    (setq modes (delete item modes)
          headers (append headers (cons item nil))))
  (setq-default mode-line-format modes
                header-line-format headers))

(add-hook 'focus-in-hook #'my/mini-screen-setup-maybe)

(easy-menu-define bot-menu global-map
  "Menu for useful commands."
  '("Bot"
    ["toggle sshd" my/toggle-sshd]))

(setq select-enable-clipboard nil
      overriding-text-conversion-style nil
      temporary-file-directory my/termux-tmp-directory
      android-pass-multimedia-buttons-to-system t)

(dolist (path '(".aria2/" ".gitconfig" ".gnupg/" ".ssh/"))
  (make-symbolic-link (file-name-concat
                       my/termux-root-directory "home" path)
                      (file-name-concat "~" path) t))

(define-key key-translation-map (kbd "<delete>") (kbd "<escape>"))
(define-key key-translation-map (kbd "<deletechar>") (kbd "<escape>"))
(keymap-global-set "H-x" 'clipboard-kill-region)
(keymap-global-set "H-c" 'clipboard-kill-ring-save)
(keymap-global-set "H-v" 'clipboard-yank)

(provide 'init-android)
;;; init-android.el ends here

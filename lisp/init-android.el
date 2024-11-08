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

;; Setup for android device.

;;; Code:


;; which-func

(with-eval-after-load 'which-func
  (setq which-func-display 'header))


;; org

(with-eval-after-load 'org-protocol
  (define-advice org-protocol-check-filename-for-protocol
      (:filter-args (args) compat-android)
    "Remove prefix when called by intent."
    (let ((h (getenv "HOME"))
          (f (car args)))
      (when (string-prefix-p (format "%s/%s:/" h org-protocol-the-protocol) f)
        (setcar args (substring f (1+ (length h)))))
      args)))


;; hardware

(defun my/mini-screen-setup-maybe ()
  "Setup for mini screen mobile."
  (if (= (display-pixel-height) 260)
      (modifier-bar-mode -1)
    (modifier-bar-mode 1)))


;; dired

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

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
  ;; (start-process "fooview-run" nil "am" "start"
  ;;                (format "intent:#Intent;action=com.fooview.android.intent.RUN_WORKFLOW;component=com.fooview.android.fooview/.ShortcutProxyActivity;S.action=%s;end" cmd))
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


;; sshd

(defvar my/sshd-pid-file
  (file-name-concat my/termux-root-directory "usr/var/run/sshd.pid")
  "sshd pid file.")

(defvar my/sshd-timer nil
  "sshd timer object.")

(defun my/toggle-sshd ()
  "Toggle local sshd server."
  (interactive)
  (if (process-attributes (or (my/get-pid-from-file my/sshd-pid-file) 0))
      (progn
        (delete-file my/sshd-pid-file)
        (call-process "pkill" nil nil nil "sshd")
        (when my/sshd-timer
          (cancel-timer my/sshd-timer)))
    (call-process "sshd")
    (setq my/sshd-timer (run-at-time 300 300 #'my/sshd-handler))))

(defun my/sshd-handler ()
  "Kill sshd when no connection over 5 min."
  (if-let* ((pid (or (my/get-pid-from-file my/sshd-pid-file) 0))
            (process-attributes pid))
      (with-temp-buffer
        (call-process "logcat" nil (current-buffer) nil
                      "-s" "sshd:*" "-d" "-v" "epoch")
        (search-backward " I sshd    : Server listening on " nil t)
        (let (child)
          (while (re-search-forward "^ *\\([.[:digit:]]+\\) +\\([[:digit:]]+\\).+? I sshd    : \\(Accepted\\|Disconnected from\\)" nil t)
            (if (string= (match-string 3) "Accepted")
                (push (match-string 2) child)
              (setq child (delete (match-string 2) child))))
          (when (and (null child)
                     (< (string-to-number (or (match-string 1) "0"))
                        (- (float-time) 300)))
            (signal-process pid 9)
            (delete-file my/sshd-pid-file)
            (cancel-timer my/sshd-timer))))
    (call-process "pkill" nil nil nil "sshd")
    (cancel-timer my/sshd-timer)
    (delete-file my/sshd-pid-file)))

(defvar my/dropbear-timer nil
  "dropbear timer object.")

(defvar my/dropbear-buffer-name "*dropbear*"
  "Default dropbear buffer name.")

(defun my/toggle-dropbear ()
  "Toggle local dropbear server."
  (interactive)
  (if (buffer-live-p (get-buffer my/dropbear-buffer-name))
      (let ((kill-buffer-query-functions nil))
        (call-process-shell-command "pkill dropbear")
        (kill-buffer my/dropbear-buffer-name)
        (cancel-timer my/dropbear-timer))
    (start-process "dropbear" my/dropbear-buffer-name "dropbear" "-F" "-w" "-s")
    (setq my/dropbear-timer (run-at-time 300 300 #'my/dropbear-handler
                                         (format-time-string "%G %b %d %T")))))

(defun my/dropbear-handler (exit)
  "Kill dropbear when no connection over 5 min."
  (with-current-buffer my/dropbear-buffer-name
    (goto-char (point-min))
    (let (child)
      (while (re-search-forward "^\\[\\([[:digit:]]+\\)\\] \\([[:alpha:]]\\{3\\} [[:digit:]]\\{2\\} [[:digit:]:]\\{8\\}\\) \\(Child\\|Exit\\)" nil t)
        (if (string= (match-string 3) "Child")
            (push (match-string 1) child)
          (setq child (delete (match-string 1) child)
                exit (concat (format-time-string "%G ")
                             (match-string 2)))))
      (when (and (eq child nil)
                 (time-less-p
                  (time-add
                   (encode-time
                    (parse-time-string exit))
                   300)
                  nil))
        (my/toggle-dropbear)))))


(setenv "SSH_AUTH_SOCK" (string-trim-right (shell-command-to-string "gpgconf -L agent-ssh-socket")))

(add-hook 'focus-in-hook #'my/mini-screen-setup-maybe)

(defun my/stop-record-tracks ()
  "Stop OpenTracks."
  (interactive)
  (my/rish-run "am force-stop de.dennisguse.opentracks"))

(easy-menu-define bot-menu global-map
  "Menu for useful commands."
  '("Bot"
    ["Stop Tracks" my/stop-record-tracks]
    ["toggle sshd" my/toggle-sshd]
    ["Start alist" my/net-start-alist]))

(setq select-enable-clipboard nil
      overriding-text-conversion-style nil
      temporary-file-directory my/termux-tmp-directory
      Info-default-directory-list `(,(file-name-concat my/termux-root-directory "usr/share/info/"))
      android-pass-multimedia-buttons-to-system t)

(dolist (path '(".aria2/" ".gitconfig" ".gnupg/" ".ssh/" ".config/"))
  (make-symbolic-link (file-name-concat
                       my/termux-root-directory "home" path)
                      (file-name-concat "~" path) t))

(setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config/"))

(define-key key-translation-map (kbd "<delete>") (kbd "<escape>"))
(define-key key-translation-map (kbd "<deletechar>") (kbd "<escape>"))
(keymap-global-set "H-x" 'clipboard-kill-region)
(keymap-global-set "H-c" 'clipboard-kill-ring-save)
(keymap-global-set "H-v" 'clipboard-yank)

(provide 'init-android)
;;; init-android.el ends here

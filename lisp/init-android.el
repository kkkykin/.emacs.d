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

(defun za/mini-screen-setup-maybe ()
  "Setup for mini screen mobile."
  (if (= (display-pixel-height) 260)
      (modifier-bar-mode -1)
    (modifier-bar-mode 1)))


;; dired

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode))

;; todo
(defun za/mpv-intent (scheme &optional sub)
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

(defun za/fooview-run (cmd)
  "Run fooview action."
  ;; (start-process "fooview-run" nil "am" "start"
  ;;                (format "intent:#Intent;action=com.fooview.android.intent.RUN_WORKFLOW;component=com.fooview.android.fooview/.ShortcutProxyActivity;S.action=%s;end" cmd))
  (start-process "fooview-run" nil "am" "start" "-a"
                 "com.fooview.android.intent.RUN_WORKFLOW" "-e" "action"
                 cmd "com.fooview.android.fooview/.ShortcutProxyActivity"))

(defun za/rish-run (cmd)
  "Run command with rish."
  (start-process "rish-run" nil "rish" "-c" cmd))

(defun za/normal-keyboard ()
  "Enable normal keyboard on android."
  (za/rish-run "ime set com.samsung.android.honeyboard/.service.HoneyBoardService"))

(defun za/bare-keyboard ()
  "Enable bare keyboard on android."
  (za/rish-run "ime set keepass2android.keepass2android/keepass2android.softkeyboard.KP2AKeyboard"))


;; sshd

(defvar za/sshd-pid-file
  (file-name-concat zr-termux-root-directory "usr/var/run/sshd.pid")
  "sshd pid file.")

(defvar za/sshd-timer nil
  "sshd timer object.")

(defun za/toggle-sshd ()
  "Toggle local sshd server."
  (interactive)
  (if (process-attributes (or (zr-get-pid-from-file za/sshd-pid-file) 0))
      (progn
        (delete-file za/sshd-pid-file)
        (call-process "pkill" nil nil nil "sshd")
        (when za/sshd-timer
          (cancel-timer za/sshd-timer)))
    (call-process "sshd")
    (setq za/sshd-timer (run-at-time 300 300 #'za/sshd-handler))))

(defun za/sshd-handler ()
  "Kill sshd when no connection over 5 min."
  (if-let* ((pid (or (zr-get-pid-from-file za/sshd-pid-file) 0))
            ((process-attributes pid)))
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
            (delete-file za/sshd-pid-file)
            (cancel-timer za/sshd-timer))))
    (call-process "pkill" nil nil nil "sshd")
    (cancel-timer za/sshd-timer)
    (delete-file za/sshd-pid-file)))

(defvar za/dropbear-timer nil
  "dropbear timer object.")

(defvar za/dropbear-buffer-name "*dropbear*"
  "Default dropbear buffer name.")

(defun za/toggle-dropbear ()
  "Toggle local dropbear server."
  (interactive)
  (if (buffer-live-p (get-buffer za/dropbear-buffer-name))
      (let ((kill-buffer-query-functions nil))
        (call-process-shell-command "pkill dropbear")
        (kill-buffer za/dropbear-buffer-name)
        (cancel-timer za/dropbear-timer))
    (start-process "dropbear" za/dropbear-buffer-name "dropbear" "-F" "-w" "-s")
    (setq za/dropbear-timer (run-at-time 300 300 #'za/dropbear-handler
                                         (format-time-string "%G %b %d %T")))))

(defun za/dropbear-handler (exit)
  "Kill dropbear when no connection over 5 min."
  (with-current-buffer za/dropbear-buffer-name
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
        (za/toggle-dropbear)))))


;; termux

(defun za/parse-json-program (program &rest args)
  (with-temp-buffer
    (apply #'call-process program nil t nil args)
    (goto-char (point-min))
    (json-parse-buffer)))

(defun za/termux-am (&rest args)
  (apply #'start-process " *termux-am*" nil "termux-am" args))

(defun za/termux-battery-status ()
  (za/parse-json-program "termux-battery-status"))

(defun za/termux-notification-list ()
  (za/parse-json-program "termux-notification-list"))

(defun za/termux-wifi-connectioninfo ()
  (za/parse-json-program "termux-wifi-connectioninfo"))

(defun za/termux-wifi-scaninfo ()
  (za/parse-json-program "termux-wifi-scaninfo"))

(cl-defun za/termux-toast
    (text &optional (bgcolor "gray") (color "white") (gravity "middle") short)
  "termux-toast wrapper."
  (let ((args (list "-b" bgcolor "-c" color "-g" gravity text)))
    (and short (push "-s" args))
    (apply #'start-process "termux-toast" nil "termux-toast" args)))

(cl-defun za/termux-notifications-notify
    ( &key title body replaces-id timeout urgency app-icon
      image-path suppress-sound category on-close actions led-color
      led-off led-on group channel action ongoing alert-once)
  "Send notification via termux-notification with specified parameters.
TITLE is the notification title
BODY is the notification content
REPLACES-ID is the ID to replace existing notification
TIMEOUT specifies when to remove the notification (need REPLACES-ID)
Other parameters map to termux-notification CLI options."
  (let (args)
    (cl-macrolet ((add-arg (key val)
                    `(when ,val (setq args (append (list ,key ,val) args)))))
      (add-arg "-t" title)
      (add-arg "-c" body)
      (add-arg "-i" replaces-id)
      (add-arg "--priority" urgency)
      (add-arg "--icon" app-icon)
      (add-arg "--image-path" image-path)
      (add-arg "--type" category)
      (add-arg "--on-delete" on-close)
      (add-arg "--led-color" led-color)
      (add-arg "--led-off" led-off)
      (add-arg "--led-on" led-on)
      (add-arg "--group" group)
      (add-arg "--channel" channel)
      (add-arg "--action" action)
    
      (seq-do-indexed
       (lambda (b i)
         (add-arg (format "--button%d-action" (1+ i)) (cadr b))
         (add-arg (format "--button%d" (1+ i)) (car b)))
       (seq-take (seq-split actions 2) 3))
    
      (unless suppress-sound
        (push "--sound" args))
      (when ongoing
        (push "--ongoing" args))
      (when alert-once
        (push "--alert-once" args)))
  
    (let ((proc (apply #'start-process "termux-notification" nil
                       "termux-notification" args)))
      ;; Set up removal timer if timeout specified
      (when (and proc timeout replaces-id)
        (run-with-timer
         timeout nil
         (lambda ()
           (start-process "remove-termux-notification" nil
                          "termux-notification-remove" replaces-id))))
      proc)))


(setenv "SSH_AUTH_SOCK" (string-trim-right (shell-command-to-string "gpgconf -L agent-ssh-socket")))

(add-hook 'focus-in-hook #'za/mini-screen-setup-maybe)

(defun za/stop-record-tracks ()
  "Stop OpenTracks."
  (interactive)
  (za/rish-run "am force-stop de.dennisguse.opentracks"))

(easy-menu-define bot-menu global-map
  "Menu for useful commands."
  '("Bot"
    ["Stop Tracks" za/stop-record-tracks]
    ["toggle sshd" za/toggle-sshd]
    ["Start alist" zr-net-start-alist]))

(setq select-enable-clipboard nil
      shell-file-name (expand-file-name "usr/bin/bash" zr-termux-root-directory)
      overriding-text-conversion-style nil
      temporary-file-directory zr-termux-tmp-directory
      Info-default-directory-list `(,(file-name-concat zr-termux-root-directory "usr/share/info/"))
      android-pass-multimedia-buttons-to-system t)

(dolist (path '(".aria2/" ".gitconfig" ".gnupg/" ".ssh/" ".config/"))
  (make-symbolic-link (file-name-concat
                       zr-termux-root-directory "home" path)
                      (file-name-concat "~" path) t))

(setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config/"))

(define-key key-translation-map (kbd "<delete>") (kbd "<escape>"))
(define-key key-translation-map (kbd "<deletechar>") (kbd "<escape>"))
(keymap-global-set "H-x" 'clipboard-kill-region)
(keymap-global-set "H-c" 'clipboard-kill-ring-save)
(keymap-global-set "H-v" 'clipboard-yank)


;; modifier-bar

(defun za/event-apply-prefix (key)
  "Apply the appropriate prefix to the given key.
If KEY is an array, it is converted to a key sequence.
If KEY is a number or marker, it is converted to a vector."
  (pcase key
    ((pred arrayp)
     (vconcat (listify-key-sequence key)))
    ((pred number-or-marker-p)
     (vector key))))

(defun za/event-apply-meta-prefix (_)
  "Apply the meta prefix to the event.
This function uses `meta-prefix-char` as the key."
  (za/event-apply-prefix meta-prefix-char))

(defun za/event-apply-keyboard-quit (_)
  "Apply the keyboard quit prefix (C-g) to the event."
  (za/event-apply-prefix "\C-g"))

(defun za/event-apply-cx-prefix (_)
  "Apply the C-x prefix to the event."
  (za/event-apply-prefix "\C-x"))

(defun za/event-apply-cc-prefix (_)
  "Apply the C-c prefix to the event."
  (za/event-apply-prefix "\C-c"))

(defun za/event-apply-ch-prefix (_)
  "Apply the C-h prefix to the event."
  (za/event-apply-prefix "\C-h"))

(defun za/event-apply-mark-prefix (_)
  "Apply the mark prefix (C-@) to the event."
  (za/event-apply-prefix "\C-@"))

(defun za/modifier-bar-setup ()
  "Set up the modifier bar for `modifier-bar-mode`.
This function defines custom tool-bar items and key bindings."
  (when modifier-bar-mode
    (setcdr secondary-tool-bar-map
            (append
             (cdr secondary-tool-bar-map)
             `((quit menu-item "Keyboard Quit"
                     keyboard-quit
                     :help "Keyboard Quit"
                     :image ,(tool-bar--image-expression "symbols/cross_16"))
               (c-x menu-item "C-x"
                    keyboard-quit
                    :help "C-x"
                    :image ,(tool-bar--image-expression "symbols/plus_16"))
               (c-c menu-item "C-c"
                    keyboard-quit
                    :help "C-c"
                    :image ,(tool-bar--image-expression "symbols/menu_16"))
               (help menu-item "help"
                     keyboard-quit
                     :help "help"
                     :image ,(tool-bar--image-expression "symbols/heart_16"))
               (mark menu-item "mark"
                     keyboard-quit
                     :help "mark"
                     :image ,(tool-bar--image-expression "symbols/star_16")))))
    (define-key input-decode-map [tool-bar meta]
                #'za/event-apply-meta-prefix)
    (define-key input-decode-map [tool-bar quit]
                #'za/event-apply-keyboard-quit)
    (define-key input-decode-map [tool-bar c-x]
                #'za/event-apply-cx-prefix)
    (define-key input-decode-map [tool-bar c-c]
                #'za/event-apply-cc-prefix)
    (define-key input-decode-map [tool-bar help]
                #'za/event-apply-ch-prefix)
    (define-key input-decode-map [tool-bar mark]
                #'za/event-apply-mark-prefix)))

(add-hook 'modifier-bar-mode-hook #'za/modifier-bar-setup)

(provide 'init-android)
;;; init-android.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("za/" . "zr-android-"))
;; End:

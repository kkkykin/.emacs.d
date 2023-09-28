;;; init-android.el --- Personal Android Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar my/termux
  "/data/data/com.termux/files"
  "termux root path")

;; todo
(defun my/mpv-intent (scheme &optional sub)
  "http://mpv-android.github.io/mpv-android/intent.html
am: [-e|--es <EXTRA_KEY> <EXTRA_STRING_VALUE> ...]
filepath scheme: rtmp, rtmps, rtp, rtsp, mms, mmst, mmsh, tcp, udp,
          content, file, http, https
example: am start -n is.xyz.mpv/is.xyz.mpv.MPVActivity -e filepath file:///sdcard/Music/local

optional:
- decode_mode (Byte): if set to 2, hardware decoding will be disabled
- subs (ParcelableArray of Uri): list of subtitle URIs to be added as additional tracks
- subs.enable (ParcelableArray of Uri): specifies which of the subtitles should be selected by default, subset of previous array
- position (Int): starting point of video playback in milliseconds "
  (when (file-exists-p scheme)
    (setq scheme (concat "file://" scheme)))
  (start-process "" nil "am" "start" "-n"
                 "is.xyz.mpv/is.xyz.mpv.MPVActivity"
                 "-e" "filepath" scheme
                 )
  )

(defun my/run-fooview (cmd)
  "运行 fooview 任务, 输出到 /sdcard/fooview.out，以后可以通过 emacs 自带的 httpd 回传"
  (shell-command
   (concat "am broadcast -p com.fooview.android.fooview -a runcommand -e cmd '"
           cmd " > /sdcard/fooview.out'"))
  )

(setenv "SSH_AUTH_SOCK"
        (substring
         (shell-command-to-string
          "gpgconf --homedir /data/data/com.termux/files/home/.gnupg --list-dirs agent-ssh-socket")
         0 -1))

(global-set-key (kbd "H-SPC") (kbd "<tab>"))

(set-face-attribute 'default nil :font "LXGW WenKai Mono" :height 200)

(setq touch-screen-display-keyboard t
      newsticker-hide-old-items-in-newsticker-buffer t
      archive-7z-program "7z"
      initial-buffer-choice (lambda ()
                              (org-agenda-list 1)
                              (get-buffer "*Org Agenda*")))

(use-package dired
  :hook
  (dired-mode . (lambda ()
                  (dired-hide-details-mode)))
  :bind
  (:map dired-mode-map
	("<mouse-2>" . dired-mouse-find-file))
  )

(use-package epg
  :custom
  (epg-gpg-home-directory "/data/data/com.termux/files/home/.gnupg")
  (epg-pinentry-mode 'loopback)
  :config
  ;; fix gnupg hang
  (fset 'epg-wait-for-status 'ignore))

(use-package menu-bar
  :custom
  (menu-bar-mode t))

(use-package tool-bar
  :custom
  (tool-bar-mode t)
  (modifier-bar-mode t)
  (tool-bar-position 'bottom))

(provide 'init-android)
;;; init-android.el ends here

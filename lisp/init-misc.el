;;; init-misc.el --- Misc Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar-keymap my/global-prefix-map
  :doc "A keymap for myself.")

(defvar-keymap my/mpc-prefix-map
  :doc "A keymap for mpc.")

(defvar-keymap my/org-prefix-map
  :doc "A keymap for handy global access to org helpers, particularly clocking.")

(defcustom my/light-theme-list
  '(adwaita dichromacy leuven modus-operandi-deuteranopia modus-operandi
            modus-operandi-tinted modus-operandi-tritanopia tango
            tsdh-light whiteboard)
  "Built-in light themes."
  :group 'my
  :type '(repeat symbol))

(defcustom my/dark-theme-list
  '(deeper-blue leuven-dark manoj-dark misterioso
                modus-vivendi-deuteranopia modus-vivendi
                modus-vivendi-tinted modus-vivendi-tritanopia
                tango-dark tsdh-dark wheatgrass wombat)
  "Built-in dark themes."
  :group 'my
  :type '(repeat symbol))

(defcustom my/fonts-list
  '("LXGW WenKai Mono" "Sarasa Mono SC" "Unifont-JP" "UnifontExMono")
  "prefered fonts"
  :group 'my
  :type '(repeat string))

(defcustom my/termux-root-directory "/data/data/com.termux/files"
  "Andriod termux root path."
  :group 'my
  :type 'directory)

(defcustom my/termux-tmp-direcotry (file-name-concat my/termux-root-directory "home/tmp")
  "Android termux tmp path."
  :group 'my
  :type 'directory)

(defcustom my/bookmark-shared-prefix "s/"
  "Prefix of shared bookmark name."
  :group 'my
  :type 'string)

(defcustom my/bookmark-shared-file (expand-file-name "bookmark-share" user-emacs-directory)
  "Shared bookmark file cross device."
  :group 'my
  :type 'file)

(defun my/advice-bookmark-save (orig-fun &rest args)
  "Do not save shared bookmarks to local bookmark file."
  (with-temp-buffer 
    (insert-file-contents my/bookmark-shared-file)
    (let ((ori-shared (bookmark-alist-from-buffer))
          (new-local (copy-sequence bookmark-alist))
          new-shared)
      (dolist (bm new-local)
        (when (string-prefix-p my/bookmark-shared-prefix (car bm))
          (setq new-local (delq bm new-local))
          (setq new-shared (cons bm new-shared))))
      (when-let ((sorted (sort new-shared #'eq))
                 (need-update-p (not (equal sorted ori-shared)))
                 (bookmark-alist sorted))
        (funcall orig-fun nil my/bookmark-shared-file nil))
      (let ((bookmark-alist new-local))
        (apply orig-fun args)))))

(defun my/system-dark-mode-enabled-p ()
  "Check if dark-mode is enabled."
  (pcase system-type
    ('windows-nt
     (string-search "0x1" (shell-command-to-string "reg query HKCU\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize /v AppsUseLightTheme")))))

(defun my/advice-shell-command-coding-fix (orig-fun &rest args)
  "Fix coding system for cmdproxy shell."
  (if-let* ((command (cond ((string= "*Find*" (car args)) (caddr args))
                           (t (car args))))
            (program-name
             (seq-some
              (lambda (x) (and (string-match-p "^[^\\(start\\|/.+\\)]" x) x))
              (split-string-shell-command command)))
            (need-fix (member program-name
                              `(,find-program "busybox" "ffmpeg" "mpv"))))
      (let ((process-coding-system-alist
             `(("cmdproxy" utf-8 . ,locale-coding-system))))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun my/set-theme (theme)
  "Set one theme only."
  (unless (memq theme custom-known-themes)
    (load-theme theme))
  (pcase theme
    ('adwaita
     (custom-theme-set-faces theme '(hl-line ((t (:extend t :background "navajo white"))))))
    ('whiteboard
     (custom-theme-set-faces theme '(hl-line ((t (:extend t :background "wheat"))))))
    ('tango
     (custom-theme-set-faces theme '(hl-line ((t (:extend t :background "cornsilk")))))))
  (dolist (item custom-enabled-themes)
    (disable-theme item))
  (enable-theme theme))

(defun my/shuffle-set-theme (theme-list)
  "Shuffle set theme."
  ;; (interactive
  ;;  (list (completing-read "Load theme list: "
  ;;                         '(my/light-theme-list
  ;;                           my/dark-theme-list))))
  (let* ((themes (remove (car custom-enabled-themes) theme-list))
         (theme (cl-loop for item = (seq-random-elt themes)
                         always themes
                         do (delete item themes)
                         when (memq item (custom-available-themes))
                         return item)))
    (my/set-theme theme)
    (message "Current theme: %s" (symbol-name theme))))

(defun my/setup-faces ()
  "Randomize setup faces."
  (when (display-graphic-p)
    (let* ((fonts (remove (face-attribute 'default :family) my/fonts-list))
           (font (cl-loop for ft = (seq-random-elt fonts)
                          always fonts
                          do (delete ft fonts)
                          when (find-font (font-spec :name ft))
                          return ft))
           (size (cond ((string= font "LXGW WenKai Mono") '("26" "14" "18"))
                       ((string= font "Sarasa Mono SC") '("24" "14" "17"))
                       ((string-prefix-p "Unifont" font) '("26" "14" "18"))))
           (height (cond ((string= font "LXGW WenKai Mono") '(198 108 140))
                         ((string= font "Sarasa Mono SC") '(188 108 130))
                         ((string-prefix-p "Unifont" font) '(198 108 142)))))
      ;; (add-to-list 'default-frame-alist
      ;;              `(font . ,(cond ((< (display-pixel-width) 1920) (concat font "-" (car size)))
      ;;                              ((> (display-pixel-width) 1920) (concat font "-" (caddr size)))
      ;;                              (t (concat font "-" (cadr size))))))
      (set-face-attribute 'default nil :font font :height
                          (cond ((< (display-pixel-width) 1920) (car height))
                                ((> (display-pixel-width) 1920) (caddr height))
                                (t (cadr height)))))

    (my/shuffle-set-theme my/light-theme-list)))

(defun my/advice-silence-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN.
https://scripter.co/using-emacs-advice-to-silence-messages-from-functions"
  (let ((inhibit-message t)    ;Don't show the messages in Echo area
        (message-log-max nil)) ;Don't show the messages in the *Messages* buffer
    (apply orig-fun args)))

(defun my/run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\system32\\bash.exe"))
    (shell "*bash*")))

(defun my/toggle-shell ()
  "Toggle shell between wsl bash and cmd"
  (interactive)
  (if (string= shell-file-name "C:\\Windows\\system32\\bash.exe")
      (setq shell-file-name my/vanilla-shell)
    (setq my/vanilla-shell shell-file-name
          shell-file-name "C:\\Windows\\system32\\bash.exe")))

(defun my/rclone-quit ()
  "Exit rclone safely."
  (interactive)
  (when-let* ((args '("rc" "core/quit"))
              (auth (car (auth-source-search :max 1 :host "rclone.localhost")))
              (args (append args `(,(format "--rc-user=%s" (plist-get auth :user))
                                   ,(format "--rc-pass=%s" (auth-info-password auth))))))
    (apply #'start-process "rclone-quit" nil "rclone" args)))

(defun my/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))

(defun my/adb-am (action activity)
  "ADB am command."
  (start-process "adb-am" nil "adb" "shell" "am" action activity))

(defun my/am-start-activity (name)
  "Start activity through adb."
  (let ((activity (cond ((string= name "termux")
                         "com.termux/com.termux.HomeActivity"))))
    (my/adb-am "start-activity" activity)))

(defun my/am-force-stop (name)
  "Force-stop app through adb."
  (let ((package (cond ((string= name "termux") "com.termux"))))
    (my/adb-am "force-stop" package)))

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

(defun my/sqlite-view-file-magically ()
  "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it.
From https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/"
  (let ((file-name buffer-file-name))
    (kill-current-buffer)
    (sqlite-mode-open-file file-name)))

(defun my/dired-dwim ()
  "start process with current file"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (when (eq filename nil)
      (setq filename default-directory))
    (my/mpv-intent filename))
  )

(defun my/mpv-image ()
  "mpv play current directory with miniplayer"
  (interactive)
  (async-shell-command
   (concat
    "mpv --ontop --autofit=30% --geometry=100%:20% --shuffle --image-display-duration=60 --no-osc --no-osd-bar --cursor-autohide=no --no-input-cursor \""
    default-directory "\"")
   "mpv"))

(defun my/dired-duplicate-file (arg)
  "Duplicate a file from dired with an incremented number.
If ARG is provided, it sets the counter.
https://www.emacs.dyerdwelling.family/emacs/20231013153639-emacs--more-flexible-duplicate-thing-function/"
  (interactive "p")
  (let* ((file (dired-get-file-for-visit))
         (dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (base-name (file-name-sans-extension name))
         (extension (file-name-extension name t))
         (counter (if arg (prefix-numeric-value arg) 1))
         (new-file))
    (while (and (setq new-file
                      (format "%s%s_%03d%s" dir base-name counter extension))
                (file-exists-p new-file))
      (setq counter (1+ counter)))
    (if (file-directory-p file)
        (copy-directory file new-file)
      (copy-file file new-file))
    (dired-revert)))

(defun my/advice-image-dired-create-thumb-maybe-gs (oldfun &rest args)
  (when (string= (file-name-extension (car args)) "pdf")
    (let ((image-dired-cmd-create-thumbnail-program "gs")
          (image-dired-cmd-create-thumbnail-options '("-sDEVICE=jpeg" "-dSAFER" "-r20" "-o" "%t" "%f")))
      (apply oldfun args))))

(defun my/default-callback (&rest args)
  "Test default callback."
  (message (if args "Up" "Down")))

(provide 'init-misc)
;;; init-misc.el ends here

;;; init-local.el --- Personal Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :hook
  ((prog-mode . display-line-numbers-mode)
   (text-mode . visual-line-mode))
  :bind-keymap
  ("C-x j" . my-global-prefix-map)
  :bind
  (:map global-map
	([remap eval-expression] . pp-eval-expression)
	([remap eval-last-sexp] . pp-eval-last-sexp)
	("C-c d" . 'duplicate-dwim)
	)
  (:map my-global-prefix-map
	("r" . 'rename-visited-file)
	("p" . 'delete-pair)
	("u" . 'raise-sexp)
	)

  :custom
  (system-time-locale "C")
  (use-package-always-defer t)
  (truncate-lines t)
  (set-mark-command-repeat-pop t)
  (scroll-bar-mode nil)
  (blink-cursor-mode nil)
  (column-number-mode t)
  (use-short-answers t)
  (word-wrap-by-category t)
  (custom-file "~/.emacs.d/custom.el")
  (what-cursor-show-names t)
  (kill-read-only-ok t)
  (kill-do-not-save-duplicates t)
  (duplicate-region-final-position 1)
  (indent-tabs-mode nil)
  (tab-width 4)
  (switch-to-buffer-obey-display-actions t)
  (bookmark-save-flag 1)
  ;; (mouse-1-click-follows-link -450 "click set point, long press do action")
  (reb-re-syntax 'string)
  (sentence-end-double-space nil "Fix archaic defaults")
  (eshell-scroll-to-bottom-on-input 'this)
  (comint-scroll-to-bottom-on-input 'this)
  (comint-scroll-to-bottom-on-output t)

  ;; long line performance https://emacs-china.org/t/topic/25811/9
  (bidi-display-reordering nil)
  (bidi-inhibit-bpa t)
  (long-line-threshold 1000)
  (large-hscroll-threshold 1000)
  (syntax-wholeline-max 1000)

  :config

  (prefer-coding-system 'utf-8)

  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))

  (defvar my-global-prefix-map (make-sparse-keymap)
    "A keymap for myself.")

  (defvar my/fonts-list '("LXGW WenKai Mono" "Sarasa Mono SC" "Unifont-JP" "UnifontExMono")
    "prefered fonts")

  (defun setup-faces ()
    "Randomize setup faces."
    (when (display-graphic-p)
      (let* ((fonts (remove (face-attribute 'default :family) my/fonts-list))
	     (font (cl-loop for ft = (seq-random-elt fonts)
			    always fonts
			    do (delete ft fonts)
			    when (find-font (font-spec :name ft))
			    return ft))
	     (height (cond ((string= font "LXGW WenKai Mono") '(198 108 140))
			   ((string= font "Sarasa Mono SC") '(188 108 130))
			   ((string-prefix-p "Unifont" font) '(198 108 142)))))
	(set-face-attribute 'default nil :font font :height
			    (cond ((< (display-pixel-width) 1920) (car height))
				  ((> (display-pixel-width) 1920) (car (last height)))
				  (t (cadr height)))))

      (load-theme 'leuven)
      ))

  (add-hook 'window-setup-hook #'setup-faces)

  (defun my/advice-silence-messages (orig-fun &rest args)
    "Advice function that silences all messages in ORIG-FUN.
https://scripter.co/using-emacs-advice-to-silence-messages-from-functions"
    (let ((inhibit-message t)    ;Don't show the messages in Echo area
          (message-log-max nil)) ;Don't show the messages in the *Messages* buffer
      (apply orig-fun args)))

  )

(when (eq system-type 'windows-nt)
  "setup for windowsNT"
  (setq shr-use-fonts nil
        w32-use-native-image-API t
        find-program "ind")

  (add-to-list 'exec-suffixes ".ps1")

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
  )

(when (eq system-type 'gnu/linux)
  "setup for linux"
  (defun my/transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter nil 'alpha-background value))

  (add-hook 'server-after-make-frame-hook #'setup-faces)
  )

(when (eq system-type 'android)
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
      (let ((url (concat "file://" scheme)))
        (start-process "" nil "am" "start" "-n"
                       "is.xyz.mpv/is.xyz.mpv.MPVActivity"
                       "-e" "filepath" url
                       )
        ))
    )

  (defun my/fooview-run (cmd)
    "run fooview action"
    (shell-command
     (concat "am start -a com.fooview.android.intent.RUN_WORKFLOW -e action " cmd " com.fooview.android.fooview/.ShortcutProxyActivity"))
    )

  (defun my/rish-run (cmd)
    "Run command with rish permission."
    (start-process-shell-command "" nil
                                 (concat "rish -c \"" cmd "\"")))

  (defun my/normal-keyboard ()
    "Enable normal keyboard on android."
    (my/rish-run "ime set com.samsung.android.honeyboard/.service.HoneyBoardService")
    )

  (defun my/bare-keyboard ()
    "Enable bare keyboard on android."
    (my/rish-run "ime set keepass2android.keepass2android/keepass2android.softkeyboard.KP2AKeyboard")
    )

  (dolist (hook '(focus-in-hook after-init-hook))
    (add-hook hook 'my/bare-keyboard))
  (add-hook 'focus-out-hook 'my/normal-keyboard)

  (setenv "SSH_AUTH_SOCK"
          (string-trim-right
           (shell-command-to-string
            "gpgconf --homedir /data/data/com.termux/files/home/.gnupg --list-dirs agent-ssh-socket")))

  (use-package touch-screen
    :custom
    (touch-screen-display-keyboard t)
    )

  (setq select-enable-clipboard nil)

  (keymap-global-set "C-z" 'window-swap-states)

  (keymap-global-set "§" 'clipboard-kill-region)
  (keymap-global-set "ß" 'clipboard-kill-ring-save)
  (keymap-global-set "ç" 'clipboard-yank)

  (keymap-global-set "ò" "~")
  (keymap-global-set "ó" "`")
  (keymap-global-set "ô" "|")
  (keymap-global-set "õ" "^")
  (keymap-global-set "ö" "_")
  (keymap-global-set "ō" "=")
  (keymap-global-set "œ" "\\")
  (keymap-global-set "ø" "#")

  (keymap-global-set "à" "{")
  (keymap-global-set "á" "}")
  (keymap-global-set "â" "[")
  (keymap-global-set "ã" "]")
  (keymap-global-set "ä" "<")
  (keymap-global-set "å" ">")
  (keymap-global-set "ā" "$")
  (keymap-global-set "æ" "%")
  )

(use-package menu-bar
  :config
  (unless (eq system-type 'android)
    (menu-bar-mode -1)
    )
  )

(use-package tool-bar
  :custom
  (tool-bar-button-margin 12)
  (modifier-bar-mode t)
  (tool-bar-position 'bottom)
  :config
  (unless (eq system-type 'android)
    (tool-bar-mode -1)
    )
  )

(use-package cua-base
  :hook (emacs-startup . cua-mode)
  :custom
  (cua-enable-cua-keys 'shift)
  (cua-rectangle-mark-key [(control ^)])
  (cua-prefix-override-inhibit-delay 0.3)
  (cua-delete-selection nil))

(use-package elec-pair
  :hook ((prog-mode minibuffer-mode inferior-emacs-lisp-mode) . electric-pair-local-mode)
  )

(use-package windmove
  :if (not (eq system-type 'android))
  :hook emacs-startup
  :config
  (windmove-default-keybindings 'control)
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package server :defer 5
  :if (not (eq system-type 'android))
  :config
  (unless (server-running-p)
	(server-start))
  )

(use-package grep
  :config
  (when (eq system-type 'windows-nt)
    (setq grep-program "ug"
          grep-use-null-device nil
          grep-highlight-matches t))
  )

(use-package xref
  :custom
  (xref-history-storage 'xref-window-local-history)
  :config
  (when (string= grep-program "ug")
    (setq xref-search-program 'ugrep)))

(use-package abbrev
  :custom
  (abbrev-suggest t))

(use-package hl-line
  :hook (text-mode prog-mode))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (insert-default-directory nil)
  (resize-mini-windows t)
  (history-delete-duplicates t)
  :hook
  ((emacs-startup . minibuffer-electric-default-mode)
   (emacs-startup . savehist-mode))
  )

(use-package icomplete
  :hook (emacs-startup . fido-mode)
  )

(use-package help
  :custom
  (help-window-select t "Switch to help buffers automatically")
  (help-window-keep-selected t)
  (apropos-sort-by-scores t)
  :bind
  (:map help-map
	("A" . 'apropos-function)
	("V" . 'apropos-variable)
	("B" . 'apropos-value)
	("j" . 'apropos-local-variable)
	("J" . 'apropos-local-value)
	("D" . 'info-apropos)
	("M" . 'describe-keymap)
	("z" . 'shortdoc)
	("Z" . 'apropos-library)
	)
  )

(use-package desktop
  :hook (emacs-startup . desktop-save-mode)
  :custom
  (desktop-auto-save-timeout 600)
  )

(use-package project
  :custom
  (project-kill-buffers-display-buffer-list t))

(use-package python
  :custom
  (python-shell-dedicated t))

(use-package flymake
  :hook sh-mode)

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer))
  :hook
  (ibuffer-mode . (lambda ()
                    (ibuffer-switch-to-saved-filter-groups "default")))
  :custom
  (ibuffer-show-empty-filter-groups nil)
  :config
  (setq ibuffer-saved-filter-groups
	'(("default"
           ("dired"
            (mode . dired-mode))
           ("perl"
            (mode . cperl-mode))
           ("planner"
            (or
             (name . "^\\*Calendar\\*$")
             (name . "^diary$")
             (mode . org-journal-mode)
             (mode . muse-mode)))
           ("emacs"
            (or
             (name . "^\\*scratch\\*$")
             (name . "^\\*Messages\\*$")))
           ("gnus"
            (or
             (mode . message-mode)
             (mode . bbdb-mode)
             (mode . mail-mode)
             (mode . gnus-group-mode)
             (mode . gnus-summary-mode)
             (mode . gnus-article-mode)
             (name . "^\\.bbdb$")
             (name . "^\\.newsrc-dribble")))
           ("newsticker"
            (name . "^\\*Newsticker")))))
  )

(use-package newsticker :defer 20
  :custom
  (newsticker-obsolete-item-max-age 864000)
  (newsticker-treeview-date-format "%y.%m.%d, %H:%M")
  (newsticker-url-list-defaults nil)
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-hide-old-items-in-newsticker-buffer t "plainview only")
  (newsticker-retrieval-interval 1800)
  (newsticker-retrieval-method 'extern)
  (newsticker-wget-name "curl")
  (newsticker-wget-arguments '("-qsLm30"))
  :config
  (load "init-rss.el.gpg" t t)

  (defun my/advice-newsticker-start (&optional _do-not-complain-if-running)
    (let ((running (newsticker-running-p)))
      (unless running
        (newsticker--cache-read))
      ;; start retrieval timers -- one timer for each feed
      (let ((counter 0))
        (dolist (feed (append newsticker-url-list-defaults newsticker-url-list))
          (setq counter (+ counter 1))
          (run-at-time (* counter 10) nil 'newsticker--start-feed feed)))
      (unless running
        (run-hooks 'newsticker-start-hook)
        (message "Newsticker started!"))))
  ;; with interval
  (advice-add 'newsticker-start :before-until #'my/advice-newsticker-start)
  ;; remove message: Error while retrieving image | news from feeds
  (dolist (fn '(newsticker--image-sentinel newsticker--sentinel-work))
    (advice-add fn :around #'my/advice-silence-messages))
  
  (newsticker-start t)
  )

(use-package eww
  :custom
  (url-cookie-trusted-urls '())
  (url-cookie-untrusted-urls '(".*"))
  (eww-auto-rename-buffer 'title)
  (shr-inhibit-images t "images maybe hang in newsticker")
  :hook
  (eww-mode . (lambda ()
                (setq-local shr-inhibit-images nil)))
  :config

  ;; refine github experience: from https://emacstalk.codeberg.page/post/018/
  (setq my/url-redirect-list `(("^https://github.com/\\(.+\\)/commit/\\(\\w+\\)$" .
				;; 针对单个 commit
				(lambda (url)
                                  (format "https://github.com/%s/commit/%s.patch"
                                          (match-string 1 url)
                                          (match-string 2 url))))
                               ("^https://github.com/\\(.+\\)/pull/\\([[:digit:]]+\\)$" .
				;; 针对单个 Pull Request
				(lambda (url)
                                  (format "https://github.com/%s/pull/%s.patch"
                                          (match-string 1 url)
                                          (match-string 2 url))))
                               ("^https://github.com/\\(.+\\)/blob/\\(.+\\)" .
				;; 针对单个文件
				(lambda (url)
                                  (format "https://github.com/%s/raw/%s"
                                          (match-string 1 url)
                                          (match-string 2 url))))))

  (defun my/advice-url-redirect (fn url &rest args)
    (catch 'ret
      (dolist (redirect-rule my/url-redirect-list)
	(let* ((regexp (car redirect-rule))
               (redirect-fn (cdr redirect-rule))
               (inhibit-message t))
          (when-let* ((matched-groups (string-match regexp url)))
            (setq url (funcall redirect-fn url))
            (message "Redirect URL to %s" url)
            (throw 'ret url)))))
    (apply fn url args))

  (advice-add 'eww :around 'my/advice-url-redirect)

  (defun my/eww-render-hook()
    (let ((url (plist-get eww-data :url)))
      (cond
       ((string-suffix-p ".patch" url) (diff-mode))
       ((string-suffix-p ".el" url) (emacs-lisp-mode))
       ((string-suffix-p ".rs" url) (rust-mode))
       ((string-suffix-p ".go" url) (go-mode))
       (t (when (and (plist-get eww-data :source)
                     ;; 排除微信公众号内的文章
                     (not (string-match-p "weixin\\.qq\\.com" url)))
            (eww-readable))))))

  (add-hook 'eww-after-render-hook 'my/eww-render-hook)
  )

(use-package tramp
  :custom
  (tramp-use-scp-direct-remote-copying t)
  :config
  (connection-local-set-profile-variables
   'tramp-connection-local-termux-profile
   `((tramp-remote-path
      . ,(mapcar
          (lambda (x)
            (if (stringp x) (concat "/data/data/com.termux/files" x) x))
          (copy-tree tramp-remote-path)))))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux") "remote-shell" "sh"))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux")
                     "tmpdir" "/data/data/com.termux/files/home/tmp"))
  (connection-local-set-profiles
   '(:application tramp :user "termux")
   'tramp-connection-local-termux-profile)
  )

(use-package shell
  :custom
  (shell-kill-buffer-on-exit t))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-l")
  (dired-mouse-drag-files t)
  (delete-by-moving-to-trash t)
  (wdired-allow-to-change-permissions 'advanced)
  (wdired-use-interactive-rename t)
  :init
  (when (eq system-type 'android)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("× μ" . my/mpv-image)
              ("C-c d" . my/dired-duplicate-file)
              ("e" . my/dired-dwim)
              ("<mouse-2>" . dired-mouse-find-file))
  :config
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
      default-directory "\"") "mpv" ))

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

  (setq archive-7z-program (let ((7z (or (executable-find "7z")
                                         (executable-find "7za")
                                         (executable-find "7zz"))))
                             (when 7z
                               (file-name-base 7z))))
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.\\(rar\\|zip\\|7z\\)\\(\\.001\\)?$"
               (concat archive-7z-program " x -aoa"))
         (list "\\.apk$" "adb install")
         (list "\\(?:\\.t\\(?:\\(?:ar\\.\\)?zst\\)\\)\\'"
               "zstd -dc ? | tar -xf -")
         (list "\\.\\(mp4\\|mkv\\|avi\\|webm\\|flv\\|m4v\\|mov\\)\\'"
               "ffmpeg -hide_banner -y -strict 2 -hwaccel auto -i ? -vf \"scale='min(2560,iw)':-1\" -c:v hevc_nvenc -rc vbr -cq 19 -qmin 19 -qmax 19 -profile:v main10 -pix_fmt p010le -b:v 0K -bf:v 3 -b_ref_mode middle -temporal-aq 1 -rc-lookahead 32 -c:a libopus -b:a 128k -f mp4 ff-`?`")
         (list "\\.\\(png\\|jpe?g\\|gif\\|webp\\|bmp\\)\\'"
               "ffmpeg -hide_banner -y -i ? -vf \"scale='min(4096,iw)':-1\" -c:v libaom-av1 -cpu-used 6 -row-mt 1 -tiles 2x2 -still-picture 1 -crf 20 -f avif ff-`?`")))
  )

(use-package dired-aux
  :custom
  (dired-compress-file-default-suffix ".zst")
  (dired-compress-directory-default-suffix ".tar.zst")
  :config
  (setq dired-compress-files-alist
        (append `(("\\.\\(tzst\\)\\'" . "tar -cf - %i | zstd -5 -o %o")
                  ("\\.\\(zip\\|7z\\)\\'" . ,(concat archive-7z-program " a -mx5 %o %i"))
                  )
                dired-compress-files-alist))
  (setq dired-compress-file-suffixes
        (append `(("\\.\\(zip\\|rar\\)\\'" #1=""
                   ,(concat archive-7z-program " x -aoa -o%o %i"))
                  ("\\.t\\(ar\\.\\)?\\(gz\\|xz\\|bz\\)\\'" #1#
                   ,(concat archive-7z-program " x %i -so | "
                            archive-7z-program " x -aoa -si -ttar -o%o"))
                  ("\\.t\\(ar\\.\\)?zst\\'" #1# "zstd -dc %i | tar -xf -")
                  ("\\.zst\\'" #1# "zstd -d --rm %i"))
                dired-compress-file-suffixes))
  )

(use-package arc-mode
  :config
  ;; use 7z manipulate rar archive
  (advice-add 'archive-rar-summarize :override #'archive-7z-summarize)
  (advice-add 'archive-rar-extract :override #'archive-7z-extract)
  )

(use-package dictionary
  :bind
  (:map my-global-prefix-map
        ("d" . dictionary-search))
  :custom
  (dictionary-server "dict.tw")
  (dictionary-use-single-buffer t))

(use-package epg
  :config
  (when (eq system-type 'android)
    (setq epg-gpg-home-directory "/data/data/com.termux/files/home/.gnupg"
          epg-pinentry-mode 'loopback)
    ;; fix gnupg hang
    (fset 'epg-wait-for-status 'ignore))
  )

(use-package mpc
  :bind-keymap
  ("C-c m" . mpc-global-prefix-map)
  :bind
  (:map mpc-global-prefix-map
        ("s" . 'mpc-toggle-play)
        ("n" . 'mpc-next)
        ("p" . 'mpc-prev)
        ("b" . 'mpc-status-buffer-show)
        ("g" . 'mpc-seek-current)
        ("r" . 'mpc-toggle-repeat)
        ("z" . 'mpc-toggle-shuffle)
        ("a" . 'mpc-toggle-single)
        ("u" . 'mpc-update))
  :custom
  (mpc-host "127.0.0.1")
  :config
  (defvar mpc-global-prefix-map (make-sparse-keymap)
    "A keymap for mpc."))

(use-package org
  :bind-keymap
  ("C-c o" . org-global-prefix-map)
  :bind
  (:map global-map
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  (:map org-global-prefix-map
        ("s" . org-store-link)
        ("y" . org-insert-link-global)
        ("j" . 'org-clock-goto)
        ("l" . 'org-clock-in-last)
        ("i" . 'org-clock-in)
        ("o" . 'org-clock-out))
  :custom
  (org-replace-disputed-keys t "see `'org-disputed-keys'")
  (org-use-speed-commands t)
  (org-default-notes-file "~/org/inbox.org")
  (org-agenda-files `(,org-default-notes-file
                      "~/org/todo"))
  (org-refile-use-cache nil)
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archive")
  (org-export-coding-system 'utf-8)
  :config
  (defvar org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . t)
      (ditaa . t)
      (dot . t)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . t)
      (ocaml . nil)
      (octave . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (screen . nil)
      (sh . t) ;; obsolete
      (shell . t)
      (sql . t)
      (sqlite . t))))

  (setq org-tag-alist '(
            ;; locale
            (:startgroup)
            ("home" . ?h)
            ("work" . ?w)
            (:endgroup)
            (:newline)
            ;; scale
            (:startgroup)
            ("one-shot" . ?o)
            ("project" . ?j)
            ("tiny" . ?t)
            (:endgroup)
            ;; misc
            ("meta")
            ("review")
            ("reading")))

  ;; (require 'oc-csl)                     ; citation support

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo)))
          ("w" "Work" agenda ""
           ((org-agenda-files '("work.org"))))))
  )

(use-package flyspell
  :hook text-mode)

(use-package calendar
  :custom
  (diary-file "~/org/diary"))

(use-package image
  :custom
  (image-use-external-converter t)
  :config
  (add-to-list 'image-file-name-extensions "avif")
  (unless (executable-find "gm")
    (setq image-dired-cmd-create-thumbnail-program "ffmpeg"
          image-dired-cmd-create-thumbnail-options '("-i" "%f"
                                                     "-map_metadata" "-1"
                                                     "-vf" "scale=%w:-1"
                                                     "-f" "mjpeg" "%t"))
    
    (defun my/advice-image-dired-create-thumb-maybe-gs (oldfun &rest args)
      (when (string= (file-name-extension (car args)) "pdf")
        (let ((image-dired-cmd-create-thumbnail-program "gs")
              (image-dired-cmd-create-thumbnail-options '("-sDEVICE=jpeg" "-dSAFER" "-r20" "-o" "%t" "%f")))
          (apply oldfun args))
        )
      )
    (advice-add 'image-dired-create-thumb-1 :around #'my/advice-image-dired-create-thumb-maybe-gs)
    )
  )

(use-package doc-view
  :custom
  (doc-view-resolution 300))

(use-package eglot
  :custom
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.1)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package ediff
  :custom
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package gnus
  :custom
  (gnus-home-directory "~/.emacs.d/gnus/")
  )

(use-package tab-bar :defer 2
  :custom
  (tab-bar-select-tab-modifiers '(control))
  (tab-bar-show 1)
  :config
  (tab-bar-history-mode)
  )

(use-package recentf :defer 1
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/data/data/com.termux/files/home/tmp"
                     "/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

(use-package emacs-news-mode
  :bind
  (:map emacs-news-view-mode-map
        ("n" . outline-next-visible-heading)
        ("p" . outline-previous-visible-heading)
        ("f" . outline-forward-same-level)
        ("b" . outline-backward-same-level)
        ("u" . outline-up-heading)))

(use-package url
  :config
  ;; Network Proxy: from centaur
  (defcustom centaur-proxy "127.0.0.1:10807"
    "Set HTTP/HTTPS proxy."
    :group 'centaur
    :type 'string)

  (defcustom centaur-socks-proxy "127.0.0.1:10808"
    "Set SOCKS proxy."
    :group 'centaur
    :type 'string)

  (defun proxy-http-show ()
    "Show HTTP/HTTPS proxy."
    (interactive)
    (if url-proxy-services
        (message "Current HTTP proxy is `%s'" centaur-proxy)
      (message "No HTTP proxy")))

  (defun proxy-http-enable ()
    "Enable HTTP/HTTPS proxy."
    (interactive)
    (setq url-proxy-services
          `(("http" . ,centaur-proxy)
            ("https" . ,centaur-proxy)
            ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
    (proxy-http-show))

  (defun proxy-http-disable ()
    "Disable HTTP/HTTPS proxy."
    (interactive)
    (setq url-proxy-services nil)
    (proxy-http-show))

  (defun proxy-http-toggle ()
    "Toggle HTTP/HTTPS proxy."
    (interactive)
    (if (bound-and-true-p url-proxy-services)
        (proxy-http-disable)
      (proxy-http-enable)))

  (defun proxy-socks-show ()
    "Show SOCKS proxy."
    (interactive)
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%s"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy")))

  (defun proxy-socks-enable ()
    "Enable SOCKS proxy."
    (interactive)
    (require 'socks)
    (setq url-gateway-method 'socks
          socks-noproxy '("localhost"))
    (let* ((proxy (split-string centaur-socks-proxy ":"))
           (host (car proxy))
           (port (string-to-number (cadr proxy))))
      (setq socks-server `("Default server" ,host ,port 5)))
    (setenv "all_proxy" (concat "socks5://" centaur-socks-proxy))
    (proxy-socks-show))

  (defun proxy-socks-disable ()
    "Disable SOCKS proxy."
    (interactive)
    (setq url-gateway-method 'native
          socks-noproxy nil
          socks-server nil)
    (setenv "all_proxy" "")
    (proxy-socks-show))

  (defun proxy-socks-toggle ()
    "Toggle SOCKS proxy."
    (interactive)
    (if (bound-and-true-p socks-noproxy)
        (proxy-socks-disable)
      (proxy-socks-enable))))

(use-package treesit
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (lua "https://github.com/MunifTanjim/tree-sitter-lua")
          (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
          ;; ini
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          ;; frontend
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          )))

(use-package ox-latex
  :custom
  (org-export-backends '(html latex md ascii icalendar))
  (org-latex-pdf-process '("tectonic %f"))
  (org-latex-default-class "ctexart")
  (org-latex-packages-alist '(("margin=1in,a4paper" "geometry" nil)
                              ("" "fvextra" nil)))
  :config
  (add-to-list 'org-latex-classes
               '("ctexart" "\\documentclass[utf8]{ctexart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("ctexbook" "\\documentclass[utf8]{ctexbook}"
                 ("\\part{%s}" . "\\part*{%s}")
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(package-initialize)

(use-package engrave-faces :defer nil
  :if (package-installed-p 'engrave-faces)
  :after org
  :config
  (setq org-latex-src-block-backend 'engraved)
  )

(use-package lua-ts-mode
  :mode "\\.lua\\'")

(use-package html-ts-mode
  :if (package-installed-p 'html-ts-mode)
  :mode "\\.html\\'"
  :vc (:url "https://github.com/mickeynp/html-ts-mode")
  :config
  (add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode))
  )

(use-package nix-ts-mode
  :if (package-installed-p 'nix-ts-mode)
  :mode "\\.nix\\'")

(use-package combobulate
  :if (package-installed-p 'combobulate)
  :vc (:url "https://github.com/mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c j")
  :hook
  ((tsx-ts-mode typescript-ts-mode json-ts-mode yaml-ts-mode css-ts-mode js-ts-mode python-ts-mode) . combobulate-mode)
  :init
  (when (package-installed-p 'html-ts-mode)
    (add-hook 'html-ts-mode-hook 'combobulate-mode)
    )
  )

(use-package which-key :defer 3
  :if (package-installed-p 'which-key)
  :config
  (which-key-mode))

(use-package denote
  :if (package-installed-p 'denote)
  :bind (("C-c n n" . denote)
         ("C-c n d" . denote-date)
         ("C-c n t" . denote-type)
         ("C-c n s" . denote-subdirectory)
         ("C-c n f" . denote-open-or-create)
         ("C-c n r" . denote-dired-rename-marked-files))
  :init
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("N" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :custom
  (denote-known-keywords '("emacs" "entertainment" "reading" "studying" "work"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-date-format nil)
  (denote-dired-rename-expert nil)
  :hook
  ((dired-mode . denote-dired-mode)
   (find-file . denote-link-buttonize-buffer))
  :config
  (setq denote-directory (expand-file-name "~/org/"))
  (unless (file-exists-p denote-directory)
    (make-directory denote-directory))
  )

;; Popup completion-at-point
(use-package corfu
  :if (package-installed-p 'corfu)
  :hook prog-mode
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook corfu-mode
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package sqlformat
  :if (package-installed-p 'sqlformat)
  :custom
  (sqlformat-command 'sqlfluff)
  ;; set sqlformat-args manually: '("-d" "mysql") or other
  )

(use-package envrc
  :if (package-installed-p 'envrc)
  :bind
  (:map envrc-mode-map
        ("C-c e" . envrc-command-map)))

(use-package emms
  :if (package-installed-p 'emms)
  :config
  (use-package mpvi :after emms
    :if (package-installed-p 'mpvi)
    :commands mpvi-open))

(use-package devdocs
  :if (package-installed-p 'devdocs)
  :bind
  (:map help-map
        ("u" . 'devdocs-lookup)
        )
  :hook
  ((python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.10")))))
  )

(use-package xeft
  :if (package-installed-p 'xeft))

(use-package khoj
  :if (package-installed-p 'khoj))

(use-package disk-usage
  :if (package-installed-p 'disk-usage))

(use-package anki-helper
  :if (package-installed-p 'anki-helper)
  :mode "\\.org\\'"
  :vc (:url "https://github.com/Elilif/emacs-anki-helper")
  :custom (anki-helper-default-deck "anki-helper"))

(use-package org-fc
  :if (package-installed-p 'org-fc))

(use-package ox-pandoc :defer 2
  :if (package-installed-p 'ox-pandoc)
  :after org
  :custom
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "tectonic"))))

(setq gc-cons-threshold (* 2 1000 1000))

(provide 'init-local)
;;; init-local.el ends here

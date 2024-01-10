;;; init.el --- Personal Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))
(require 'init-misc)

(use-package emacs
  :hook
  ((text-mode . visual-line-mode)
   (window-setup . my/setup-faces))
  :bind-keymap
  ("C-x j" . my/global-prefix-map)
  :bind
  (:map global-map
        ([remap eval-expression] . pp-eval-expression)
        ([remap eval-last-sexp] . pp-eval-last-sexp)
        ([remap upcase-word] . upcase-dwim)
        ([remap downcase-word] . downcase-dwim)
        ([remap capitalize-word] . capitalize-dwim)
        ("C-c d" . 'duplicate-dwim))
  (:map my/global-prefix-map
        ("p" . 'delete-pair)
        ("u" . 'raise-sexp)
        ("t" . 'transpose-sentences)
        ("T" . 'transpose-paragraphs)
        ("'" . 'my/insert-quotations)
        ("\"" . 'my/insert-quotes)
        ("<" . 'my/insert-than-sign)
        ("[" . 'my/insert-squarebracket)
        ("{" . 'my/insert-curlybracket))
  :custom
  (inhibit-splash-screen t)
  (initial-major-mode 'fundamental-mode)
  (system-time-locale "C")
  (use-package-always-defer t)
  (truncate-lines t)
  (mark-ring-max 6)
  (global-mark-ring-max 8)
  (set-mark-command-repeat-pop t)
  (scroll-bar-mode nil)
  (blink-cursor-mode nil)
  (column-number-mode t)
  (global-prettify-symbols-mode t)
  (prettify-symbols-unprettify-at-point t)
  (display-line-numbers-type 'relative)
  (use-short-answers t)
  (word-wrap-by-category t)
  (custom-file "~/.emacs.d/custom.el")
  (what-cursor-show-names t)
  (redisplay-skip-fontification-on-input t)
  (kill-read-only-ok t)
  (kill-do-not-save-duplicates t)
  (duplicate-region-final-position 1)
  (indent-tabs-mode nil)
  (tab-width 4)
  (switch-to-buffer-obey-display-actions t)
  (shell-command-dont-erase-buffer 'beg-last-out)
  (async-shell-command-buffer 'rename-buffer)
  (async-shell-command-display-buffer nil)
  (shell-command-default-error-buffer "*Shell Command Error*")
  ;; (mouse-1-click-follows-link -450 "click set point, long press do action")
  (reb-re-syntax 'string)

  ;; long line performance https://emacs-china.org/t/topic/25811/9
  (bidi-display-reordering nil)
  (bidi-inhibit-bpa t)
  (long-line-threshold 1000)
  (large-hscroll-threshold 1000)
  (syntax-wholeline-max 1000)
  :config
  (prefer-coding-system 'utf-8)

  (when my/sys-winnt-p
    "setup for windowsNT"
    (setq shr-use-fonts nil)
    (setenv "HOME" (file-name-parent-directory user-emacs-directory))

    (add-to-list 'exec-suffixes ".ps1"))

  (when my/sys-linux-p
    "setup for linux"
    (add-hook 'server-after-make-frame-hook #'my/setup-faces))

  (when my/sys-android-p
    (setenv "SSH_AUTH_SOCK" (string-trim-right (shell-command-to-string "gpgconf --homedir /data/data/com.termux/files/home/.gnupg --list-dirs agent-ssh-socket")))

    ;; (dolist (hook '(focus-in-hook after-init-hook))
    ;;   (add-hook hook 'my/bare-keyboard))
    ;; (add-hook 'focus-out-hook 'my/normal-keyboard)

    (setq select-enable-clipboard nil
          mode-line-format (delq 'mode-line-buffer-identification mode-line-format)
          android-pass-multimedia-buttons-to-system t)

    (define-key key-translation-map (kbd "<delete>") (kbd "ESC"))
    (define-key key-translation-map (kbd "<deletechar>") (kbd "ESC"))
    (keymap-global-set "H-x" 'clipboard-kill-region)
    (keymap-global-set "H-c" 'clipboard-kill-ring-save)
    (keymap-global-set "H-v" 'clipboard-yank)))

(use-package touch-screen
  :if my/sys-android-p
  :custom
  (touch-screen-display-keyboard t))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  (history-delete-duplicates t)
  (completion-styles '(initials partial-completion flex))
  (completion-cycle-threshold 10)
  (completions-max-height 20)
  (completions-detailed t)
  (completions-group t)

  ;; ver 30
  (minibuffer-visible-completions t)
  (minibuffer-regexp-mode t)
  :hook
  ((emacs-startup . minibuffer-electric-default-mode)
   (emacs-startup . savehist-mode)))

(use-package icomplete
  :hook (emacs-startup . fido-mode))

(use-package completion-preview
  :if (package-installed-p 'completion-preview)
  :hook (prog-mode eshell-mode inferior-emacs-lisp-mode))

(use-package help
  :custom
  (help-window-select t "Switch to help buffers automatically")
  (help-window-keep-selected t)
  (apropos-sort-by-scores t)
  :bind
  (:map help-map
        ("A" . 'apropos-user-option)
        ("V" . 'apropos-value)
        ("D" . 'info-apropos)
        ("M" . 'describe-keymap)
        ("z" . 'shortdoc)
        ("Z" . 'apropos-library)))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*eldoc for" display-buffer-at-bottom
                 (window-height . 4))))

(use-package man
  :custom
  (Man-switches "-a"))

(use-package bookmark
  :custom
  (bookmark-save-flag nil)
  :config
  (let ((shared (file-name-concat user-emacs-directory "bookmark-share")))
    (when (file-exists-p shared)
      (bookmark-load shared nil t))))

(use-package cua-base
  :hook (emacs-startup . cua-mode)
  :custom
  (cua-enable-cua-keys nil "cua-selection-mode")
  (cua-rectangle-mark-key [(control ^)])
  (cua-prefix-override-inhibit-delay 0.3)
  (cua-delete-selection nil))

(use-package display-fill-column-indicator
  :unless my/sys-android-p
  :hook prog-mode
  :custom
  (indicate-buffer-boundaries 'left)
  (display-fill-column-indicator-character ?\u254e))

(use-package time :defer 9
  :unless my/sys-android-p
  :custom
  (display-time-24hr-format t)
  (display-time-use-mail-icon t)
  (display-time-default-load-average nil)
  :config
  (display-time))

(use-package battery :defer 10
  :unless my/sys-android-p
  :config
  (when (and battery-status-function
             (not (string= "unknown" 
                           (battery-format "%B"
                                           (funcall battery-status-function)))))
    (display-battery-mode 1)))

(use-package hl-line
  ;; maybe break table.el based on text-mode
  :hook (org-mode prog-mode))

(use-package display-line-numbers
  :unless my/sys-android-p
  :hook prog-mode)

(use-package subword
  :unless my/sys-android-p
  :hook prog-mode)

(use-package glasses
  :unless my/sys-android-p
  ;; :hook prog-mode
  :custom
  (glasses-uncapitalize-p t)
  (glasses-separate-parentheses-p nil))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
  (isearch-repeat-on-direction-change t))

(use-package files
  :bind
  (:map my/global-prefix-map
        ("R" . 'rename-visited-file))
  :custom
  (save-abbrevs 'silently)
  (major-mode-remap-alist
   '((yaml-mode . yaml-ts-mode)
     (bash-mode . bash-ts-mode)
     (javascript-mode . js-ts-mode)
     (typescript-mode . typescript-ts-mode)
     (json-mode . json-ts-mode)
     (css-mode . css-ts-mode)
     (python-mode . python-ts-mode)))
  (kept-new-versions 3)
  (kept-old-versions 1)
  (version-control t)
  (delete-old-versions t)
  (backup-directory-alist
   `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t))))

(use-package menu-bar
  :config
  (unless my/sys-android-p
    (menu-bar-mode -1)))

(use-package tool-bar
  :custom
  (tool-bar-button-margin 12)
  (tool-bar-position 'bottom)
  :config
  (if my/sys-android-p
      (modifier-bar-mode)
    (tool-bar-mode -1)))

(use-package speedbar
  :bind
  (:map my/global-prefix-map
        ("b" . 'speedbar)))

(use-package electric
  :custom
  (electric-pair-mode t)
  :hook ((text-mode fundamental-mode) . (lambda () (electric-pair-local-mode -1)))
  :config
  (electric-pair-mode)
  (electric-layout-mode))

(use-package windmove
  :unless my/sys-android-p
  :hook emacs-startup
  :custom
  (windmove-create-window t)
  (windmove-wrap-around t)
  :config
  (windmove-default-keybindings)
  (windmove-display-default-keybindings)
  (windmove-delete-default-keybindings)
  (windmove-swap-states-default-keybindings '(shift control)))

(use-package server :defer 5
  :unless my/sys-android-p
  :custom
  (server-client-instructions nil)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package window
  :config
  (when my/sys-android-p
    (keymap-global-set "C-z" 'window-swap-states)))

(use-package diff
  :custom
  (diff-add-log-use-relative-names t))

(use-package grep
  :init
  (when my/sys-winnt-p
    (setq find-program "ind"
          grep-program "ug"
          grep-use-null-device nil
          grep-highlight-matches t)))

(use-package xref
  :custom
  (xref-history-storage 'xref-window-local-history)
  (xref-auto-jump-to-first-xref 'move)
  :config
  (when (string= grep-program "ug")
    (setq xref-search-program 'ugrep)))

(use-package abbrev
  :custom
  (abbrev-suggest t))

(use-package desktop
  :hook (emacs-startup . desktop-save-mode)
  :custom
  (desktop-restore-frames nil)
  (desktop-restore-eager nil)
  (desktop-auto-save-timeout 600))

;; https://karthinks.com/software/it-bears-repeating/
;; https://karthinks.com/software/a-consistent-structural-editing-interface/
;; https://karthinks.com/software/persistent-prefix-keymaps-in-emacs/
(use-package repeat
  :hook emacs-startup)

(use-package elide-head
  :unless my/sys-android-p
  :hook prog-mode)

;; https://karthinks.com/software/simple-folding-with-hideshow/
(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)
         ((ediff-prepare-buffer vc-before-checkin) . turn-off-hideshow)))

(use-package paren
  :custom
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay))

(use-package which-func :defer 5
  :config
  (which-function-mode))

(use-package midnight :defer 60
  :custom
  (midnight-delay 14400)
  (clean-buffer-list-kill-buffer-gnames
   '("*Help*" "*Apropos*" "*Completions*"
     "*Ibuffer*" "*Buffer List*" "*buffer-selection*" "*timer-list*"
     "*Compile-Log*" "*Pp Eval Output*" "*Async-native-compile-log*"
     "*Async Shell Command*" "*Shell Command Output*"
     "*Directory*" "*Calculator*" "*Calc Trail*"
     "*vc*" "*vc-diff*" "*diff*" "*xref*"))
  (clean-buffer-list-kill-regexps
   '("\\`\\*Man " "\\`\\*Shortdoc "
     "\\`\\*Newsticker " "\\`\\*newsticker-wget-image-"))
  :bind
  (:map my/global-prefix-map
        ("k" . 'clean-buffer-list))
  :config
  (midnight-mode))

(use-package project
  ;; Dispatch menu not available on android sometimes.
  ;; :config
  ;; (when my/sys-android-p
  ;;   (setq project-switch-commands 'project-find-file))
  ;;   (setopt project-switch-commands #'project-prefix-or-any-command)
  :custom
  (project-vc-include-untracked nil)
  (project-kill-buffers-display-buffer-list t))

(use-package vc
  :custom
  (vc-display-status 'no-backend)
  (vc-handled-backends '(Git SVN))
  (vc-command-messages 'log))

(use-package add-log
  :custom
  (add-log-keep-changes-together t)
  (change-log-version-info-enabled t))

(use-package python
  :custom
  (python-indent-block-paren-deeper t)
  (python-shell-dedicated t))

(use-package flymake
  :hook sh-mode)

(use-package gud
  :custom
  (gud-highlight-current-line t))

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
            (name . "^\\*Newsticker"))))))

(use-package newsticker :defer 20
  :bind
  (:map newsticker-treeview-mode-map
        ("DEL" . 'my/newsticker-treeview-prev-page))
  :custom
  (newsticker-obsolete-item-max-age 864000)
  (newsticker-treeview-date-format "%y.%m.%d, %H:%M")
  (newsticker-url-list-defaults nil)
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-hide-old-items-in-newsticker-buffer t "plainview only")
  (newsticker-retrieval-interval 1800)
  (newsticker-retrieval-method 'extern)
  (newsticker-wget-name "curl")
  (newsticker-wget-arguments `("-Lkqsm40" "--doh-url" ,my/doh-server))
  :config

  (unless (file-exists-p (file-name-concat newsticker-dir "saved"))
    (make-directory (file-name-concat newsticker-dir "saved") t))

  (load "init-rss.el.gpg" t t)

  ;; with interval
  (advice-add 'newsticker-start :before-until #'my/advice-newsticker-start)
  ;; remove message: Error while retrieving image | news from feeds
  (dolist (fn '(newsticker--image-sentinel newsticker--sentinel-work))
    (advice-add fn :around #'my/advice-silence-messages))
  (advice-add 'newsticker--image-download-by-url :around #'my/advice-url-retrieve-with-timeout)
  (advice-add 'newsticker--get-news-by-wget :filter-args #'my/advice-newsticker--get-news-by-wget)
  (advice-add 'newsticker-save-item :before-until #'my/advice-newsticker-save-item)

  (newsticker-start t))

(use-package eww
  :custom
  (eww-search-prefix "https://reverse.zzzr.eu.org/https/html.duckduckgo.com/html/?q=" "https://www.mojeek.com/search?q= or https://wiby.org/?q=")
  (eww-auto-rename-buffer 'title)
  :hook (eww-after-render . my/eww-render-hook)
  :config
  (advice-add 'eww :around 'my/advice-url-redirect))

(use-package webjump
  :bind
  (:map my/global-prefix-map
        ("/" . 'webjump))
  :config
  (dolist (web '(("Ecosia" .
                  [simple-query "www.ecosia.org"
                                "www.ecosia.org/search?method=index&q=" ""])
                 ("NixPackage" .
                  [simple-query "search.nixos.org/packages"
                                "search.nixos.org/packages?from=0&size=50&sort=relevance&type=packages&query=" ""])
                 ("NixOption" .
                  [simple-query "search.nixos.org/options"
                                "search.nixos.org/options?from=0&size=50&sort=relevance&type=packages&query=" ""])))
    (add-to-list 'webjump-sites web)))

(use-package browse-url
  :custom
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

(use-package goto-addr
  ;; :unless my/sys-android-p
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)))

(use-package tramp
  :bind
  (:map my/global-prefix-map
        ("c" . 'tramp-cleanup-connection)
        ("C" . 'tramp-cleanup-some-buffers))
  :custom
  (tramp-verbose 2)
  (tramp-use-scp-direct-remote-copying t)
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  ;; (tramp-histfile-override nil)
  :config
  (when my/sys-winnt-p
    (setq tramp-default-method "sshx"))

  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux") "remote-shell" "/data/data/com.termux/files/usr/bin/bash"))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux")
                     "tmpdir" "/data/data/com.termux/files/home/tmp"))
  (connection-local-set-profile-variables
   'tramp-connection-local-termux-profile
   `((tramp-remote-path
      . ,(mapcar
          (lambda (x)
            (if (stringp x) (concat "/data/data/com.termux/files" x) x))
          (copy-tree tramp-remote-path)))
     (explicit-shell-file-name
      . "/data/data/com.termux/files/usr/bin/bash")))
  (connection-local-set-profiles
   '(:application tramp :user "termux")
   'tramp-connection-local-termux-profile))

(use-package shell
  :custom
  (shell-completion-execonly nil)
  (shell-completion-fignore '("~" "#" "%"))
  (shell-get-old-input-include-continuation-lines t)
  (shell-kill-buffer-on-exit t))

(use-package sqlite-mode
  :magic ("SQLite format 3\x00" . my/sqlite-view-file-magically))

(use-package dired
  :custom
  (dired-maybe-use-globstar t)
  (dired-dwim-target t)
  (dired-listing-switches "-lh")
  (dired-mouse-drag-files t)
  (delete-by-moving-to-trash t)
  (wdired-allow-to-change-permissions 'advanced)
  (wdired-use-interactive-rename t)
  :init
  (when my/sys-android-p
    (add-hook 'dired-mode-hook 'dired-hide-details-mode))
  :bind (:map dired-mode-map
              ("× μ" . my/mpv-image)
              ("C-c d" . my/dired-duplicate-file)
              ("f" . my/dired-dwim)
              ("<mouse-2>" . dired-mouse-find-file))
  :config
  (require 'dired-x)
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
               "ffmpeg -hide_banner -y -i ? -vf \"scale='min(4096,iw)':-1\" -c:v libaom-av1 -cpu-used 6 -row-mt 1 -tiles 2x2 -still-picture 1 -crf 20 -f avif ff-`?`"))))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep 't)
  (dired-vc-rename-file t)
  (dired-isearch-filenames 'dwim)
  (dired-compress-file-default-suffix ".zst")
  (dired-compress-directory-default-suffix ".tar.zst")
  :config
  (setq dired-compress-files-alist
        (append `(("\\.\\(tzst\\)\\'" . "tar -cf - %i | zstd -5 -o %o")
                  ("\\.\\(zip\\|7z\\)\\'" . ,(concat archive-7z-program " a -mx5 %o %i")))
                dired-compress-files-alist))
  (setq dired-compress-file-suffixes
        (append `(("\\.\\(zip\\|rar\\)\\'" #1=""
                   ,(concat archive-7z-program " x -aoa -o%o %i"))
                  ("\\.t\\(ar\\.\\)?\\(gz\\|xz\\|bz\\)\\'" #1#
                   ,(concat archive-7z-program " x %i -so | "
                            archive-7z-program " x -aoa -si -ttar -o%o"))
                  ("\\.t\\(ar\\.\\)?zst\\'" #1# "zstd -dc %i | tar -xf -")
                  ("\\.zst\\'" #1# "zstd -d --rm %i"))
                dired-compress-file-suffixes)))

(use-package arc-mode
  :config
  ;; use 7z manipulate rar archive
  (advice-add 'archive-rar-summarize :before-until #'archive-7z-summarize)
  (advice-add 'archive-rar-extract :before-until #'archive-7z-extract))

(use-package dictionary
  :bind
  (:map my/global-prefix-map
        ("d" . dictionary-search))
  :custom
  (dictionary-server "dict.tw")
  (dictionary-use-single-buffer t))

(use-package epg
  :config
  (unless my/sys-linux-p
    (setq epg-pinentry-mode 'loopback))
  (when my/sys-android-p
    (setq epg-gpg-home-directory "/data/data/com.termux/files/home/.gnupg")
    ;; fix gnupg hang
    (fset 'epg-wait-for-status 'ignore)))

(use-package mpc
  :bind-keymap
  ("C-c m" . my/mpc-prefix-map)
  :bind
  (:map my/mpc-prefix-map
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
  (mpc-host "127.0.0.1"))

(use-package avoid :defer 15
  :unless my/sys-android-p
  :config
  (mouse-avoidance-mode 'exile))

;; https://karthinks.com/software/different-strokes-for-different-folks/
(use-package strokes
  :unless my/sys-android-p
  :bind ("<down-mouse-3>" . 'strokes-do-stroke))

(use-package timeclock
  :bind
  (:map my/global-prefix-map
        ("o" . 'timeclock-in)
        ("O" . 'timeclock-out)))

(use-package flyspell
  :hook (text-mode
         (prog-mode . flyspell-prog-mode))
  :custom
  (ispell-personal-dictionary (expand-file-name "dict.txt" user-emacs-directory)))

(use-package calendar
  :custom
  (calendar-chinese-all-holidays-flag t))

(use-package image
  :custom
  (image-dired-external-viewer "mpv")
  (image-use-external-converter t)
  :config
  (add-to-list 'image-file-name-extensions "avif")
  (unless (executable-find "gm")
    (setq image-dired-cmd-create-thumbnail-program "ffmpeg"
          image-dired-cmd-create-thumbnail-options '("-i" "%f"
                                                     "-map_metadata" "-1"
                                                     "-vf" "scale=%w:-1"
                                                     "-f" "mjpeg" "%t"))
    
    (advice-add 'image-dired-create-thumb-1 :around #'my/advice-image-dired-create-thumb-maybe-gs)))

(use-package doc-view
  :custom
  (doc-view-scale-internally nil)
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
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (unless my/sys-android-p
    (setq ediff-split-window-function 'split-window-horizontally)))

(use-package message
  :custom
  (message-kill-buffer-on-exit t)
  (message-signature nil))

(use-package rmail
  :custom
  (rmail-remote-password-required t)
  (send-mail-command 'smtpmail-send-it))

(use-package gnus
  :custom
  (message-send-mail-function 'smtpmail-send-it)
  (gnus-home-directory "~/.emacs.d/gnus/"))

(use-package remember
  :custom
  (remember-diary-file (file-name-concat org-directory "remember"))
  :custom
  (add-to-list 'remember-handler-functions 'remember-diary-extract-entries))

(use-package pcomplete)
(use-package forms)
(use-package ses)
(use-package todo-mode)
(use-package skeleton)

(use-package tab-bar :defer 2
  :custom
  (tab-bar-select-tab-modifiers '(control))
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  :custom-face
  (tab-bar ((t (:inherit mode-line :box nil))))
  (tab-bar-tab ((t (:inherit mode-line :box nil))))
  (tab-bar-tab-inactive ((t (:inherit mode-line-inactive :box nil))))
  :config
  (tab-bar-history-mode))

(use-package recentf :defer 1
  :bind
  (:map my/global-prefix-map
        ("r" . 'recentf))
  :config
  (recentf-mode)
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/data/data/com.termux/files/home/tmp" "/tmp/" "/ssh:"
                     "/sshx:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

(use-package esh-mode
  :bind
  (:map my/global-prefix-map
        ("e" . eshell))
  :custom
  (eshell-scroll-to-bottom-on-output 'others)
  :config
  (add-to-list 'eshell-modules-list 'eshell-smart))

(use-package comint
  :custom
  (comint-input-autoexpand 'input)
  (comint-insert-previous-argument-from-end t)
  (comint-prompt-read-only t)
  (comint-input-ignoredups t)
  (comint-completion-recexact t)
  (comint-completion-autolist t)
  (comint-scroll-to-bottom-on-output 'others)
  :config
  (dolist (hook '(comint-strip-ctrl-m
                  comint-truncate-buffer
                  comint-osc-process-output))
      (add-hook 'comint-output-filter-functions hook)))

(use-package outline
  :custom
  (outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode-cycle t)
  :config
  (require 'foldout))

(use-package emacs-news-mode
  :bind
  (:map emacs-news-view-mode-map
        ("n" . outline-next-visible-heading)
        ("p" . outline-previous-visible-heading)
        ("f" . outline-forward-same-level)
        ("b" . outline-backward-same-level)
        ("u" . outline-up-heading)))

(use-package url
  :custom
  (url-privacy-level 'high)
  (url-mime-language-string "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6")
  (url-cookie-trusted-urls '())
  (url-cookie-untrusted-urls '(".*"))
  :config
  (dolist (fn '(url-retrieve
                url-queue-retrieve
                url-retrieve-internal
                url-retrieve-synchronously))
    (advice-add fn :around #'my/advice-url-retrieve-with-proxy)))

(use-package filesets :defer 10
  :unless my/sys-android-p
  :config
  (filesets-init))

(use-package filecache
  :config
  (setq file-cache-alist '(("init.el" "~/.emacs.d/")
                           ("default.el" "~/.emacs.d/lisp/")
                           ("pip.ini" "~/.config/pip/")
                           ("mpv.conf" "~/.config/mpv/")
                           ("config" "~/.config/yt-dlp/"))))

(use-package shadowfile :defer 11
  :unless my/sys-android-p
  :config
  (shadow-initialize))

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
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))))

(use-package custom
  :custom
  (custom-buffer-done-kill t))

(use-package org
  :init (setq org-directory "~/org")
  :bind-keymap
  ("C-c o" . my/org-prefix-map)
  :bind
  (:map my/org-prefix-map
        ("b" . org-fold-hide-block-toggle))
  :custom
  (org-replace-disputed-keys t "see `'org-disputed-keys'")
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-cycle-emulate-tab 'whitestart)
  (org-use-speed-commands t)
  (org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (org-agenda-files `(,org-default-notes-file
                      ,(file-name-concat org-directory "todo")))
  (org-refile-use-cache nil)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archive")
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))
  (org-tag-alist '(
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
                   ("noexport")
                   ("meta")
                   ("review")
                   ("reading"))))

(use-package org-agenda
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :custom
  (org-agenda-diary-file (file-name-concat org-directory "diary.org"))
  (org-agenda-custom-commands
   '(("n" "Agenda and All Todos"
      ((agenda)
       (todo)))
     ("w" "Work" agenda ""
      ((org-agenda-files '("work.org")))))))

(use-package org-capture
  :bind
  (:map global-map
        ("C-c c" . org-capture))
  :custom
  (org-capture-templates
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
      "** TODO %?\n%U\n%i\n%a"))))

(use-package org-clock
  :bind
  (:map my/org-prefix-map
        ("j" . org-clock-goto)
        ("l" . org-clock-in-last)
        ("i" . org-clock-in)
        ("o" . org-clock-out)))

(use-package ol
  :bind
  (:map my/org-prefix-map
        ("s" . org-store-link)
        ("y" . org-insert-link-global))
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))

(use-package ob
  :custom
  (org-plantuml-exec-mode 'plantuml)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (seq-filter
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((R . nil)
      (ditaa . nil)
      (dot . nil)
      (emacs-lisp . t)
      (gnuplot . t)
      (haskell . nil)
      (latex . t)
      (ledger . nil)
      (ocaml . nil)
      (octave . nil)
      (plantuml . t)
      (python . t)
      (ruby . nil)
      (screen . nil)
      (shell . t)
      (sql . t)
      (sqlite . t)))))

(use-package ox
  :custom
  (org-export-coding-system 'utf-8)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts nil)
  (org-export-backends '(org html latex md ascii icalendar))
  (org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6" :rules "all" :frame "border"))
  (org-html-postamble nil)
  (org-latex-pdf-process '("tectonic %f"))
  (org-latex-default-class "ctexart")
  (org-latex-packages-alist '(("margin=1in,a4paper" "geometry" nil)
                              ("" "fvextra" nil)))
  :config
  (with-eval-after-load 'ox-latex
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
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))

(use-package package
  :custom
  ;; (package-install-upgrade-built-in t)
  ;; (package-pinned-packages nil)
  (package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                      ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")
                      ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")))
  :config
  (package-initialize))

(use-package engrave-faces :defer nil
  :if (package-installed-p 'engrave-faces)
  :after org
  :config
  (setq org-latex-src-block-backend 'engraved))

(use-package htmlize
  :if (package-installed-p 'htmlize))

(use-package lua-ts-mode
  :mode "\\.lua\\'")

(use-package nix-ts-mode
  :if (package-installed-p 'nix-ts-mode)
  :mode "\\.nix\\'")

(use-package html-ts-mode
  :if (package-installed-p 'html-ts-mode)
  :mode "\\.html\\'"
  :vc (:url "https://github.com/mickeynp/html-ts-mode")
  :config
  (add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode)))

(use-package combobulate
  :if (package-installed-p 'combobulate)
  :vc (:url "https://github.com/mickeynp/combobulate")
  :preface
  (setq combobulate-key-prefix "C-c j")
  :hook
  ((tsx-ts-mode typescript-ts-mode json-ts-mode yaml-ts-mode css-ts-mode js-ts-mode python-ts-mode) . combobulate-mode)
  :init
  (when (package-installed-p 'html-ts-mode)
    (add-hook 'html-ts-mode-hook 'combobulate-mode)))

(use-package anki-helper
  :if (package-installed-p 'anki-helper)
  :mode "\\.org\\'"
  :vc (:url "https://github.com/Elilif/emacs-anki-helper")
  :custom (anki-helper-default-deck "anki-helper"))

(use-package gnuplot
  :if (package-installed-p 'gnuplot)
  :mode ("\\.gp\\'" . gnuplot-mode))

;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :if (package-installed-p 'avy))

(use-package marginalia
  :if (package-installed-p 'marginlia)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark
  :if (package-installed-p 'embark))

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
  (denote-rename-buffer-format "[D] %t")
  :hook
  ((dired-mode . denote-dired-mode)
   (find-file . denote-link-buttonize-buffer))
  :config
  (setq denote-directory (expand-file-name "~/org/"))
  (unless (file-exists-p denote-directory)
    (make-directory denote-directory))
  (denote-rename-buffer-mode 1))

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
  (corfu-popupinfo-hide nil))

(use-package powershell
  :if (package-installed-p 'powershell))

(use-package sqlformat
  :if (package-installed-p 'sqlformat)
  :bind
  (:map sql-mode-map
        ("C-c C-f" . sqlformat))
  :custom
  ;; set sqlformat-args manually: '("-d" "mysql") or other
  (sqlformat-command 'sqlfluff))

(use-package envrc
  :if (package-installed-p 'envrc)
  :bind
  (:map envrc-mode-map
        ("C-c e" . envrc-command-map)))

(use-package simple-httpd
  :if (package-installed-p 'simple-httpd)
  :custom
  (httpd-host "127.0.0.1")
  (httpd-serve-files nil)
  :config
  ;; Maybe adb proxy
  ;; adb shell settings put global http_proxy 192.168.xx.xxx:8888
  ;; adb shell settings delete global http_proxy

  ;; (defservlet* exec text/plain (cmd)
  ;;              (insert (string-trim-right
  ;;                       (shell-command-to-string cmd))))
  (httpd-start))

(use-package emms
  :if (package-installed-p 'emms))

(use-package mpvi :after emms
  :if (package-installed-p 'mpvi)
  :commands mpvi-open)

(use-package devdocs
  :if (package-installed-p 'devdocs)
  :bind
  (:map help-map
        ("u" . 'devdocs-lookup))
  :hook
  ((python-mode . (lambda () (setq-local devdocs-current-docs '("python~3.10"))))))

(use-package xeft
  :if (package-installed-p 'xeft))

(use-package khoj
  :if (package-installed-p 'khoj))

(use-package disk-usage
  :if (package-installed-p 'disk-usage)
  :custom
  (disk-usage-find-command find-program))

(use-package aria2
  :if (package-installed-p 'aria2)
  :bind
  (:map my/global-prefix-map
        ("a" . 'aria2-downloads-list))
  :config
  (let ((auth (car (auth-source-search :host "aria2.localhost"))))
    (setq aria2-rcp-secret (auth-info-password auth)
          aria2-rcp-listen-port (string-to-number (plist-get auth :port)))))

(use-package org-fc
  :if (package-installed-p 'org-fc))

(use-package ox-pandoc :defer 2
  :if (package-installed-p 'ox-pandoc)
  :after org
  :custom
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "tectonic"))))

(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here

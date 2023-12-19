;;; init.el --- Personal Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :hook
  ((prog-mode . display-line-numbers-mode)
   (text-mode . visual-line-mode))
  :bind-keymap
  ("C-x j" . my/global-prefix-map)
  :bind
  (:map global-map
        ([remap eval-expression] . pp-eval-expression)
        ([remap eval-last-sexp] . pp-eval-last-sexp)
        ("C-c d" . 'duplicate-dwim))
  (:map my/global-prefix-map
        ("p" . 'delete-pair)
        ("u" . 'raise-sexp)
        ("'" . 'my/insert-quotations)
        ("\"" . 'my/insert-quotes)
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
  (display-line-numbers-width 3)
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
  (bookmark-save-flag 1)
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

  (defvar my/global-prefix-map (make-sparse-keymap)
    "A keymap for myself.")

  (defvar my/fonts-list '("LXGW WenKai Mono" "Sarasa Mono SC" "Unifont-JP" "UnifontExMono")
    "prefered fonts")

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

      (load-theme 'leuven)))

  (add-hook 'window-setup-hook #'my/setup-faces)

  (defun my/advice-silence-messages (orig-fun &rest args)
    "Advice function that silences all messages in ORIG-FUN.
https://scripter.co/using-emacs-advice-to-silence-messages-from-functions"
    (let ((inhibit-message t)    ;Don't show the messages in Echo area
          (message-log-max nil)) ;Don't show the messages in the *Messages* buffer
      (apply orig-fun args)))

  (defun my/insert-quotations (&optional arg)
    "Enclose following ARG sexps in quotation marks.
Leave point after open-paren."
    (interactive "P")
    (insert-pair arg ?\' ?\'))

  (defun my/insert-quotes (&optional arg)
    "Enclose following ARG sexps in quotes.
Leave point after open-quote."
    (interactive "P")
    (insert-pair arg ?\" ?\"))

  (defun my/insert-curlybracket (&optional arg)
    "Enclose following ARG sexps in curlybracket.
Leave point after open-bracket."
    (interactive "P")
    (insert-pair arg ?\{ ?\})))

(when (eq system-type 'windows-nt)
  "setup for windowsNT"
  (setq shr-use-fonts nil
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
            shell-file-name "C:\\Windows\\system32\\bash.exe"))))

(when (eq system-type 'gnu/linux)
  "setup for linux"
  (defun my/transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque:")
    (set-frame-parameter nil 'alpha-background value))

  (add-hook 'server-after-make-frame-hook #'my/setup-faces))

(when (eq system-type 'android)
  (defvar my/termux
    "/data/data/com.termux/files"
    "termux root path")

  (setenv "SSH_AUTH_SOCK" (string-trim-right (shell-command-to-string "gpgconf --homedir /data/data/com.termux/files/home/.gnupg --list-dirs agent-ssh-socket")))

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
                       "-e" "filepath" url))))

  (defun my/fooview-run (cmd)
    "run fooview action"
    (shell-command
     (concat "am start -a com.fooview.android.intent.RUN_WORKFLOW -e action " cmd " com.fooview.android.fooview/.ShortcutProxyActivity")))

  (defun my/rish-run (cmd)
    "Run command with rish permission."
    (start-process-shell-command "" nil
                                 (concat "rish -c \"" cmd "\"")))

  (defun my/normal-keyboard ()
    "Enable normal keyboard on android."
    (my/rish-run "ime set com.samsung.android.honeyboard/.service.HoneyBoardService"))

  (defun my/bare-keyboard ()
    "Enable bare keyboard on android."
    (my/rish-run "ime set keepass2android.keepass2android/keepass2android.softkeyboard.KP2AKeyboard"))

  (dolist (hook '(focus-in-hook after-init-hook))
    (add-hook hook 'my/bare-keyboard))
  (add-hook 'focus-out-hook 'my/normal-keyboard)

  (use-package touch-screen
    :custom
    (touch-screen-display-keyboard t))

  (setq select-enable-clipboard nil
        android-pass-multimedia-buttons-to-system t)

  (keymap-global-set "H-x" 'clipboard-kill-region)
  (keymap-global-set "H-c" 'clipboard-kill-ring-save)
  (keymap-global-set "H-v" 'clipboard-yank))

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  (history-delete-duplicates t)
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

(use-package cua-base
  :hook (emacs-startup . cua-mode)
  :custom
  (cua-enable-cua-keys nil "cua-selection-mode")
  (cua-rectangle-mark-key [(control ^)])
  (cua-prefix-override-inhibit-delay 0.3)
  (cua-delete-selection nil))

(use-package display-fill-column-indicator
  :unless (eq system-type 'android)
  :hook prog-mode
  :custom
  (indicate-buffer-boundaries 'left)
  (display-fill-column-indicator-character ?\u254e))

(use-package time :defer 9
  :unless (eq system-type 'android)
  :custom
  (display-time-24hr-format t)
  (display-time-use-mail-icon t)
  (display-time-default-load-average nil)
  :config
  (display-time))

(use-package battery :defer 10
  :unless (eq system-type 'android)
  :config
  (when (and battery-status-function
             (not (string= "unknown" 
                           (battery-format "%B"
                                           (funcall battery-status-function)))))
    (display-battery-mode 1)))

(use-package hl-line
  ;; maybe break table.el based on text-mode
  :hook (org-mode prog-mode))

(use-package isearch
  :custom
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
  (isearch-repeat-on-direction-change t))

(use-package files
  :bind
  (:map my/global-prefix-map
        ("r" . 'rename-visited-file))
  :custom
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
  (unless (eq system-type 'android)
    (menu-bar-mode -1)))

(use-package tool-bar
  :custom
  (tool-bar-button-margin 12)
  (modifier-bar-mode t)
  (tool-bar-position 'bottom)
  :config
  (unless (eq system-type 'android)
    (tool-bar-mode -1)))

(use-package speedbar
  :bind
  (:map my/global-prefix-map
        ("b" . 'speedbar)))

(use-package electric
  :custom
  (electric-pair-mode t)
  :hook ((text-mode fundamental-mode) . (lambda () (electric-pair-local-mode -1))))

(use-package windmove
  :unless (eq system-type 'android)
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
  :unless (eq system-type 'android)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package window
  :config
  (add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))
  (when (eq system-type 'android)
    (keymap-global-set "C-z" 'window-swap-states)))

(use-package grep
  :config
  (when (eq system-type 'windows-nt)
    (setq grep-program "ug"
          grep-use-null-device nil
          grep-highlight-matches t)))

(use-package xref
  :custom
  (xref-history-storage 'xref-window-local-history)
  :config
  (with-eval-after-load 'grep
    (when (string= grep-program "ug")
      (setq xref-search-program 'ugrep))))

(use-package abbrev
  :custom
  (abbrev-suggest t))

(use-package desktop
  :hook (emacs-startup . desktop-save-mode)
  :custom
  (desktop-auto-save-timeout 600))

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
  ;; (when (eq system-type 'android)
  ;;   (setq project-switch-commands 'project-find-file))
  ;;   (setopt project-switch-commands #'project-prefix-or-any-command)
  :custom
  (project-kill-buffers-display-buffer-list t))

(use-package vc
  :custom
  (vc-display-status 'no-backend)
  (vc-handled-backends '(SVN Git)))

(use-package python
  :custom
  (python-indent-block-paren-deeper t)
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
  (newsticker-wget-arguments `("-kqsLm30" "--doh-url" ,my/doh-server))
  :config

  (defcustom my/rss-bridge-list '("https://rss-bridge.org/bridge01/"
                                  ;; "https://rss-bridge.lewd.tech/"
                                  "https://rss.nixnet.services/"
                                  "https://wtf.roflcopter.fr/rss-bridge/"
                                  "https://rssbridge.flossboxin.org.in/")
    "Public RSS-Bridge Server."
    :group 'my
    :type '(repeat string))

  (defcustom my/rss-hub-list '("https://rsshub.zzzr.eu.org/"
                               "https://rsshub.rssforever.com/"
                               "https://rsshub.feeded.xyz/"
                               "https://hub.slarker.me/"
                               "https://rsshub.liumingye.cn/"
                               "https://rsshub-instance.zeabur.app/"
                               "https://rss.fatpandac.com/"
                               "https://rsshub.pseudoyu.com/"
                               "https://rsshub.friesport.ac.cn/"
                               "https://rsshub.atgw.io/")
    "Public RSSHub Server."
    :group 'my
    :type '(repeat string))

  (defcustom my/rss-bridge-server (car my/rss-bridge-list)
    "RSS bridge default server."
    :group 'my
    :type 'string)

  (defcustom my/rss-hub-server (car my/rss-hub-list)
    "RSS hub default server."
    :group 'my
    :type 'string)

  (defcustom my/img-cdn-server-list '("https://images.weserv.nl?url=${href_ue}"
                                      "https://imageproxy.pimg.tw/resize?url=${href_ue}")
    "Public Image CDN server."
    :group 'my
    :type '(repeat string))

  (defcustom my/img-cdn-server "https://imageproxy.pimg.tw/resize?url=${href_ue}"
    "Default Image CDN server."
    :group 'my
    :type 'string)

  (defun my/rss-bridge-generator (bridge)
    "Generate atom feed via rss-bridge."
    (concat my/rss-bridge-server
            "?action=display&format=Atom&bridge=" bridge))

  (defun my/rss-bridge-wp (blog limit &optional content)
    "Returns the newest full posts of a WordPress powered website."
    (concat (my/rss-bridge-generator "WordPressBridge")
            "&url=" (url-hexify-string blog)
            "&limit=" (number-to-string limit)
            (when content (concat "&content_selector=" (url-hexify-string content)))))
  ;; todo
  ;; (defun my/rss-bridge-filter ())

  (defun my/rss-bridge-reducer (feed percentage)
    "Choose a percentage of a feed you want to see."
    (concat (my/rss-bridge-generator "FeedReducerBridge")
            "&url=" (url-hexify-string feed)
            "&percentage=" (number-to-string percentage)))

  (defun my/rss-bridge-css-expander (feed limit content &optional
                                          content-cleanup
                                          dont-expand-metadata
                                          discard-thumbnail)
    "Expand any site RSS feed using CSS selectors."
    (concat (my/rss-bridge-generator "CssSelectorFeedExpanderBridge")
            "&feed=" (url-hexify-string feed)
            "&limit=" (number-to-string limit)
            "&content_selector=" (url-hexify-string content)
            (when content-cleanup (concat "&content_cleanup=" (url-hexify-string content-cleanup)))
            (concat "&dont_expand_metadata=" (when dont-expand-metadata "on"))
            (concat "&discard_thumbnail=" (when discard-thumbnail "on"))))

  (defun my/rss-bridge-css-selector (home limit entry load-pages &rest args)
    "Convert any site to RSS feed using CSS selectors. The bridge first
selects the element describing the article entries. It then extracts
the links to the articles from these elements. It then, depending on
the setting 'load_pages', either parses the selected elements,
or downloads the page for each article and parses those. Parsing the
elements or page is done using the provided selectors."
    (let ((content (plist-get args :content))
          (title (plist-get args :title))
          (time (plist-get args :time))
          (time-fmt (plist-get args :time-fmt))
          (url (plist-get args :url))
          (url-pattern (plist-get args :url-pattern))
          (title-cleanup (plist-get args :title-cleanup))
          (content-cleanup (plist-get args :content-cleanup))
          (cookie (plist-get args :cookie))
          (author (plist-get args :author))
          (cat (plist-get args :cat))
          (rm-style (plist-get args :rm-style)))
      (concat (my/rss-bridge-generator "CssSelectorComplexBridge")
              "&home_page=" (url-hexify-string home)
              "&limit=" (number-to-string limit)
              "&entry_element_selector=" (url-hexify-string entry)
              "&use_article_pages=" (when load-pages "on")
              (when content (concat "&article_page_content_selector=" (url-hexify-string content)))
              (when title (concat "&title_selector=" (url-hexify-string title)))
              (when time (concat "&time_selector=" (url-hexify-string time)))
              (when time-fmt (concat "&time_format=" (url-hexify-string time-fmt)))
              (when url (concat "&url_selector=" (url-hexify-string url)))
              (when url-pattern (concat "&url_pattern=" (url-hexify-string url-pattern)))
              (when title-cleanup (concat "&title_cleanup=" (url-hexify-string title-cleanup)))
              (when content-cleanup (concat "&content_cleanup=" (url-hexify-string content-cleanup)))
              (when cookie (concat "&cookie=" (url-hexify-string cookie)))
              (when author (concat "&author_selector=" (url-hexify-string author)))
              (when cat (concat "&category_selector=" (url-hexify-string cat)))
              (concat "&remove_styling=" (when rm-style "on")))))

  (defun my/rss-bridge-merger (feeds limit name)
    "This bridge merges two or more feeds into a single feed. Max 10
items are fetched from each feed."
    (concat (my/rss-bridge-generator "FeedMergeBridge")
            (let ((m 0))
              (mapconcat
               (lambda (feed)
                 (setq m (+ m 1))
                 (concat "&feed_" (number-to-string m) "="
                         (url-hexify-string feed)))
               feeds))
            "&limit=" (number-to-string limit)
            "&feed_name=" (url-hexify-string name)))

  (defun my/rss-hub-generator (router &rest args)
    "Generate feed via RSSHub."
    (let* ((fmt (plist-get args :fmt))
           (main (concat my/rss-hub-server router
                         (when fmt (concat "." fmt))))
           (url (url-generic-parse-url main))
           (limit (plist-get args :limit))
           (full (plist-get args :full))
           (brief (plist-get args :brief))
           (unsort (plist-get args :unsort))
           (opencc (plist-get args :opencc))
           (scihub (plist-get args :scihub))
           (f-uncase (plist-get args :f-uncase))
           (f (plist-get args :f))
           (f-title (plist-get args :f-title))
           (f-desc (plist-get args :f-desc))
           (f-author (plist-get args :f-author))
           (f-cat (plist-get args :f-cat))
           (f-time (plist-get args :f-time))
           (fo (plist-get args :fo))
           (fo-title (plist-get args :fo-title))
           (fo-desc (plist-get args :fo-desc))
           (fo-author (plist-get args :fo-author))
           (fo-cat (plist-get args :fo-cat))
           (img-tp (plist-get args :img-tp))
           (domain (plist-get args :domain))
           (code (auth-source-pick-first-password :host (url-host url))))
      (concat main "?"
              (string-join
               (delq
                nil
                `(,(when limit (concat "limit=" (number-to-string limit)))
                  ,(when full "mode=fulltext")
                  ,(when brief (concat "brief=" (number-to-string brief)))
                  ,(when unsort "sorted=false")
                  ,(when opencc (concat "opencc=" opencc))
                  ,(when scihub "scihub=1")
                  ,(when f-uncase "filter_case_sensitive=false")
                  ,(when f (concat "filter=" (url-hexify-string f)))
                  ,(when f-title (concat "filter_title=" (url-hexify-string f-title)))
                  ,(when f-desc (concat "filter_description=" (url-hexify-string f-desc)))
                  ,(when f-author (concat "filter_author=" (url-hexify-string f-author)))
                  ,(when f-cat (concat "filter_category=" (url-hexify-string f-cat)))
                  ,(when f-time (concat "filter_time=" (url-hexify-string f-time)))
                  ,(when fo (concat "filterout=" (url-hexify-string fo)))
                  ,(when fo-title (concat "filterout_title=" (url-hexify-string fo-title)))
                  ,(when fo-desc (concat "filterout_description=" (url-hexify-string fo-desc)))
                  ,(when fo-author (concat "filterout_author=" (url-hexify-string fo-author)))
                  ,(when fo-cat (concat "filterout_category=" (url-hexify-string fo-cat)))
                  ,(when img-tp (concat "image_hotlink_template=" (url-hexify-string my/img-cdn-server)))
                  ,(when domain (concat "domain=" (url-hexify-string domain)))
                  ,(when code (concat "code=" (md5 (concat (url-filename url) code))))))
               "&"))))

  (defun my/rss-hub-transform (url s-fmt &rest args)
    "Pass URL and transformation rules to convert HTML/JSON into RSS."
    (let ((title (plist-get args :t))
          (item (plist-get args :i))
          (item-title (plist-get args :it))
          (item-title-a (plist-get args :ita))
          (item-link (plist-get args :il))
          (item-link-a (plist-get args :ila))
          (item-desc (plist-get args :id))
          (item-desc-a (plist-get args :ida))
          (item-pub (plist-get args :ip))
          (item-pub-a (plist-get args :ipa))
          (extra (plist-get args :extra)))
      (apply
       #'my/rss-hub-generator
       (concat "rsshub/transform/" s-fmt "/" (url-hexify-string url) "/"
               (string-join
                (delq
                 nil
                 `(,(when title (concat "title=" (url-hexify-string title)))
                   ,(when item (concat "item=" (url-hexify-string item)))
                   ,(when item-title (concat "itemTitle=" (url-hexify-string item-title)))
                   ,(when item-title-a (concat "itemTitleAttr=" (url-hexify-string item-title-a)))
                   ,(when item-link (concat "itemLink=" (url-hexify-string item-link)))
                   ,(when item-link-a (concat "itemLinkAttr=" (url-hexify-string item-link-a)))
                   ,(when item-desc (concat "itemDesc=" (url-hexify-string item-desc)))
                   ,(when item-desc-a (concat "itemDescAttr=" (url-hexify-string item-desc-a)))
                   ,(when item-pub (concat "itemPubDate=" (url-hexify-string item-pub)))
                   ,(when item-pub-a (concat "itemPubDateAttr=" (url-hexify-string item-pub-a)))))
                "&"))
       extra)
      ))

  (unless (file-exists-p (file-name-concat newsticker-dir "saved"))
    (make-directory (file-name-concat newsticker-dir "saved") t))

  (load-file (file-name-concat user-emacs-directory "init-rss.el.gpg"))

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

  (defun my/advice-newsticker--get-news-by-wget (args)
    (let ((host (url-host (url-generic-parse-url (cadr args))))
          (domains my/proxy-domain)
          (wget-arguments (caddr args)))
      (catch 'aaa
        (while domains
          (if (string-match-p (car domains) host)
              (progn
                (setf (caddr args)
                      (append wget-arguments
                              `("-x"
                                ,(concat "http://emacs@"
                                         my/centaur-proxy))))
                (throw 'aaa "a"))
            (setq domains (cdr domains))))))
    args)
  (advice-add 'newsticker--get-news-by-wget :filter-args #'my/advice-newsticker--get-news-by-wget)

  (defun my/newsticker-treeview-prev-page ()
    "Scroll item buffer."
    (interactive)
    (save-selected-window
      (select-window (newsticker--treeview-item-window) t)
      (condition-case nil
          (scroll-down nil)
        (error
         (goto-char (point-max))))))

  (defun my/advice-newsticker-save-item (feed item)
    "Save FEED ITEM."
    (interactive)
    (let ((filename
           (read-string "Filename: "
                        (file-name-concat newsticker-dir
                                          "saved"
                                          (replace-regexp-in-string
                                           "[: ]+" "_"
                                           (concat feed "--"
                                                   (newsticker--title item)
                                                   ".html"))))))
      (with-temp-buffer
        (insert (newsticker--desc item))
        (write-file filename t))))
  (advice-add 'newsticker-save-item :before-until #'my/advice-newsticker-save-item)

  (newsticker-start t))

(use-package eww
  :custom
  (eww-search-prefix "https://reverse.zzzr.eu.org/https/html.duckduckgo.com/html/?q=" "https://www.mojeek.com/search?q= or https://wiby.org/?q=")
  (eww-auto-rename-buffer 'title)
  ;; (shr-inhibit-images t "images maybe hang in newsticker")
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

  (add-hook 'eww-after-render-hook 'my/eww-render-hook))

(use-package tramp
  :bind
  (:map my/global-prefix-map
        ("c" . 'tramp-cleanup-connection))
  :custom
  (tramp-verbose 2)
  (tramp-use-scp-direct-remote-copying t)
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  :config

  ;; (add-to-list 'tramp-smb-options "client min protocol=NT1")

  ;; ver 30.1
  (unless (functionp 'tramp-revert-buffer-with-sudo)
    (defun tramp-revert-buffer-with-sudo ()
      "Revert current buffer to visit with \"sudo\" permissions.
An alternative method could be chosen with `tramp-file-name-with-method'.
If the buffer visits a file, the file is replaced.
If the buffer runs `dired', the buffer is reverted."
      (interactive)
      (cond
       ((buffer-file-name)
        (find-alternate-file (tramp-file-name-with-sudo (buffer-file-name))))
       ((tramp-dired-buffer-p)
        (dired-unadvertise (expand-file-name default-directory))
        (setq default-directory (tramp-file-name-with-sudo default-directory)
              list-buffers-directory
              (tramp-file-name-with-sudo list-buffers-directory))
        (if (consp dired-directory)
            (setcar
             dired-directory (tramp-file-name-with-sudo (car dired-directory)))
          (setq dired-directory (tramp-file-name-with-sudo dired-directory)))
        (dired-advertise)
        (revert-buffer)))))

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
   'tramp-connection-local-termux-profile))

(use-package shell
  :custom
  (shell-get-old-input-include-continuation-lines t)
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
              ("f" . my/dired-dwim)
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
               "ffmpeg -hide_banner -y -i ? -vf \"scale='min(4096,iw)':-1\" -c:v libaom-av1 -cpu-used 6 -row-mt 1 -tiles 2x2 -still-picture 1 -crf 20 -f avif ff-`?`"))))

(use-package dired-aux
  :custom
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
  (when (eq system-type 'android)
    (setq epg-gpg-home-directory "/data/data/com.termux/files/home/.gnupg"
          epg-pinentry-mode 'loopback)
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
  (mpc-host "127.0.0.1")
  :config
  (defvar my/mpc-prefix-map (make-sparse-keymap)
    "A keymap for mpc."))

(use-package avoid :defer 15
  :unless (eq system-type 'android)
  :config
  (mouse-avoidance-mode 'exile))

(use-package org
  :init (setq org-directory "~/org")
  :bind-keymap
  ("C-c o" . my/org-prefix-map)
  :bind
  (:map global-map
        ("C-c a" . org-agenda)
        ("C-c c" . org-capture))
  (:map my/org-prefix-map
        ("s" . org-store-link)
        ("y" . org-insert-link-global)
        ("b" . org-hide-block-toggle)
        ("j" . 'org-clock-goto)
        ("l" . 'org-clock-in-last)
        ("i" . 'org-clock-in)
        ("o" . 'org-clock-out))
  :custom
  (org-replace-disputed-keys t "see `'org-disputed-keys'")
  (org-special-ctrl-k t)
  (org-use-speed-commands t)
  (org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (org-agenda-files `(,org-default-notes-file
                      ,(file-name-concat org-directory "todo")))
  (org-refile-use-cache nil)
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archive")
  (org-export-coding-system 'utf-8)
  :config
  (defvar my/org-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

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
            ("noexport")
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
           ((org-agenda-files '("work.org")))))))

(use-package ob
  :custom
  (org-plantuml-exec-mode 'plantuml)
  :config
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
        (sqlite . t)))))

(use-package flyspell
  :hook (text-mode
         (prog-mode . flyspell-prog-mode)))

(use-package calendar
  :custom
  (diary-file (file-name-concat org-directory "diary")))

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
          (apply oldfun args))))
    (advice-add 'image-dired-create-thumb-1 :around #'my/advice-image-dired-create-thumb-maybe-gs)))

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
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (unless (eq system-type 'android)
    (setq ediff-split-window-function 'split-window-horizontally)))

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
  :config
  (tab-bar-history-mode))

(use-package recentf :defer 1
  :config
  (recentf-mode)
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/data/data/com.termux/files/home/tmp"
                     "/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

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
  (comint-scroll-to-bottom-on-output 'others))

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
  :init

  (defun my/internet-up-p (&optional host callback)
    "Test connectivity via ping."
    (let* ((args (if (eq system-type 'windows-nt)
                     '("-n" "1" "-w" "1") '("-c1" "-W1")))
           (proc (apply #'start-process "internet-test" nil "ping"
                        (if host host "baidu.com") args))
           (callback (if callback callback
                       (lambda (&rest cbargs)
                         (message (if cbargs "Up" "Down"))))))
      (set-process-sentinel proc (lambda (proc signal)
                                   (apply callback
                                          (if (= 0 (process-exit-status proc))
                                              '(t) nil))))))

  (defun my/url-up-p (url &optional callback doh)
    "Test connectivity via curl."
    (let* ((args `("-kqsLm10" "-o/dev/null" "-w%{http_code}" ,url
                   ,@(when doh (list "--doh-url" doh))))
           (proc (apply #'start-process "url-test" nil "curl" args))
           (callback (if callback callback
                       (lambda (&rest cbargs)
                         (message (if cbargs "Up" "Down"))))))
      (set-process-filter proc (lambda (proc line)
                                 (apply callback
                                        (if (string= "200" line)
                                            '(t) nil))))))

  (defcustom my/doh-server-list '("https://1.1.1.1/dns-query"
                                  "https://1.12.12.12/dns-query"
                                  "https://223.6.6.6/dns-query")
    "Public DOH Server."
    :group 'my
    :type '(repeat string))

  (defcustom my/doh-server (car my/doh-server-list)
    "Default DOH Server."
    :group 'my
    :type 'string)

  :config

  (defcustom my/proxy-domain '("\\(\\.\\|^\\)google.com$")
    "Domain through proxy."
    :group 'my
    :type '(repeat regexp))

  ;; Network Proxy: from centaur
  (defcustom my/centaur-proxy "127.0.0.1:10807"
    "Set HTTP/HTTPS proxy."
    :group 'my
    :type 'string)

  (defcustom my/centaur-socks-proxy "127.0.0.1:10808"
    "Set SOCKS proxy."
    :group 'my
    :type 'string)

  (defun my/proxy-http-show ()
    "Show HTTP/HTTPS proxy."
    (interactive)
    (if url-proxy-services
        (message "Current HTTP proxy is `%s'" my/centaur-proxy)
      (message "No HTTP proxy")))

  (defun my/proxy-http-enable ()
    "Enable HTTP/HTTPS proxy."
    (interactive)
    (setq url-proxy-services
          `(("http" . ,my/centaur-proxy)
            ("https" . ,my/centaur-proxy)
            ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
    (my/proxy-http-show))

  (defun my/proxy-http-disable ()
    "Disable HTTP/HTTPS proxy."
    (interactive)
    (setq url-proxy-services nil)
    (my/proxy-http-show))

  (defun my/proxy-http-toggle ()
    "Toggle HTTP/HTTPS proxy."
    (interactive)
    (if (bound-and-true-p url-proxy-services)
        (my/proxy-http-disable)
      (my/proxy-http-enable)))

  (defun my/proxy-socks-show ()
    "Show SOCKS proxy."
    (interactive)
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%s"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy")))

  (defun my/proxy-socks-enable ()
    "Enable SOCKS proxy."
    (interactive)
    (require 'socks)
    (setq url-gateway-method 'socks
          socks-noproxy '("localhost"))
    (let* ((proxy (split-string my/centaur-socks-proxy ":"))
           (host (car proxy))
           (port (string-to-number (cadr proxy))))
      (setq socks-server `("Default server" ,host ,port 5)))
    (setenv "all_proxy" (concat "socks5://" my/centaur-socks-proxy))
    (my/proxy-socks-show))

  (defun proxy-socks-disable ()
    "Disable SOCKS proxy."
    (interactive)
    (setq url-gateway-method 'native
          socks-noproxy nil
          socks-server nil)
    (setenv "all_proxy" "")
    (my/proxy-socks-show))

  (defun proxy-socks-toggle ()
    "Toggle SOCKS proxy."
    (interactive)
    (if (bound-and-true-p socks-noproxy)
        (proxy-socks-disable)
      (my/proxy-socks-enable)))

  (defun my/url-http-parse-response ()
    "Parse http response, From 'https://emacs-china.org/t/elisp-http/18432/2'."
    (set-buffer-multibyte t)
    (goto-char (point-min))
    (let ((headers `(("Status"
                      . ,(progn
                           (re-search-forward "^[^:]+? \\([[:digit:]]+\\)"
                                              (pos-eol) t 1)
                           (match-string 1)))))
          body)
      (while (re-search-forward "^\\([^:]*\\): \\(.+\\)"
                                url-http-end-of-headers t)
        (push (cons (match-string 1)
                    (match-string 2))
              headers))
      (setq headers (nreverse headers))
      (goto-char (1+ url-http-end-of-headers))
      (setq body (buffer-substring (point) (point-max)))
      (list headers body))))

(use-package filesets :defer 10
  :config
  (filesets-init))

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

(use-package ox
  :custom
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

(use-package which-key :defer 3
  :if (package-installed-p 'which-key)
  :config
  (which-key-mode))

(use-package gnuplot
  :if (package-installed-p 'gnuplot)
  :mode ("\\.gp\\'" . gnuplot-mode))

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
  (defservlet* exec text/plain (cmd)
    (insert (string-trim-right
             (shell-command-to-string cmd))))
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
  :if (package-installed-p 'disk-usage))

(use-package org-fc
  :if (package-installed-p 'org-fc))

(use-package ox-pandoc :defer 2
  :if (package-installed-p 'ox-pandoc)
  :after org
  :custom
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "tectonic"))))

(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here

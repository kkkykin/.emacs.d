;;; init.el --- Personal Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))

(defconst zr-sys-winnt-p (eq system-type 'windows-nt)
  "Windows System.")

(defconst zr-sys-linux-p (eq system-type 'gnu/linux)
  "Linux System.")

(defconst zr-sys-android-p (eq system-type 'android)
  "Android System.")

(defconst zr-sys-android-gui-p (and zr-sys-android-p (display-graphic-p))
  "Android System.")

(use-package emacs
  :hook
  ((text-mode . visual-line-mode)
   (next-error . recenter))
  :bind
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  :custom
  (elisp-fontify-semantically t)
  (mode-line-compact t)
  (mode-line-frame-identification nil)
  (mode-line-client nil)
  (mode-line-front-space nil)
  (mode-line-end-spaces nil)
  (read-process-output-max (* 1024 1024))
  (inhibit-splash-screen t)
  (indicate-buffer-boundaries 'left)
  (initial-major-mode 'fundamental-mode)
  (disabled-command-function nil)
  (system-time-locale "C")
  (use-dialog-box nil)
  (use-package-enable-imenu-support t)
  (use-package-check-before-init t)
  (use-package-always-defer t)
  (truncate-lines t)
  (mark-ring-max 6)
  (global-mark-ring-max 8)
  (set-mark-command-repeat-pop t)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-conservatively 97)
  (make-cursor-line-fully-visible nil)
  (blink-matching-paren-highlight-offscreen t)
  (blink-cursor-mode nil)
  (column-number-mode t)
  (shift-select-mode nil)
  (global-prettify-symbols-mode t)
  (prettify-symbols-unprettify-at-point t)
  (use-short-answers t)
  (y-or-n-p-use-read-key t)
  (word-wrap-by-category t)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (what-cursor-show-names t)
  (redisplay-skip-fontification-on-input t)
  (kill-read-only-ok t)
  (kill-do-not-save-duplicates t)
  (duplicate-region-final-position 1)
  (indent-tabs-mode nil)
  (tab-always-indent 'complete)
  (tab-first-completion 'word-or-paren-or-punct)
  (delete-pair-blink-delay nil)
  (tab-width 4)
  (shell-command-dont-erase-buffer 'beg-last-out)
  (async-shell-command-buffer 'rename-buffer)
  (async-shell-command-display-buffer nil)
  (shell-command-default-error-buffer "*Shell Command Error*")
  (mail-user-agent 'gnus-user-agent)
  ;; (mouse-1-click-follows-link -450 "click set point, long press do action")

  ;; long line performance https://emacs-china.org/t/topic/25811/9
  (bidi-display-reordering nil)
  (bidi-inhibit-bpa t)
  (long-line-threshold 1000)
  (large-hscroll-threshold 1000)
  (syntax-wholeline-max 1000)
  :config
  (easy-menu-define zr-menu global-map
    "My useful menu."
    '("zr"
      ["scratch" scratch-buffer]))
  (defun zr-defer-gc ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun zr-do-restore-gc ()
    (setq gc-cons-threshold 80000000))
  (defun zr-restore-gc ()
    (run-at-time 1 nil #'zr-do-restore-gc))
  (put 'buffer-file-coding-system 'safe-local-variable 'symbolp)
  (put 'buffer-auto-save-file-name 'safe-local-variable 'null)
  (prefer-coding-system 'utf-8)
  (setenv "PYTHONIOENCODING" "utf-8")
  (set-charset-priority 'unicode))

;; [[https://github.com/yilkalargaw/emacs-native-snippets]]
(use-package tempo
  :autoload tempo-define-template
  :bind
  ("M-g M-n" . 'tempo-forward-mark)
  ("M-g M-p" . 'tempo-backward-mark)
  (:repeat-map zr-tempo-repeat-map
               ("n" . tempo-forward-mark)
               ("p" . tempo-backward-mark)))

(use-package transient
  :autoload
  transient--set-layout
  transient-define-prefix)

(use-package mode-local
  :autoload
  (mode-local-bind
   setq-mode-local))

(use-package multisession
  :init
  (when (and (sqlite-available-p)
             (version< "3.40" (sqlite-version)))
    (setq multisession-storage 'sqlite)))

(use-package init-misc
  :if (locate-library "init-misc")
  :demand t)

(use-package init-winnt :demand t
  :if (and zr-sys-winnt-p (locate-library "init-winnt")))

(use-package init-linux :demand t
  :if (and zr-sys-linux-p (locate-library "init-linux")))

(use-package init-android :demand t
  :if (and zr-sys-android-p (locate-library "init-android")))

(use-package init-prog
  :if (locate-library "init-prog")
  :demand t)

(use-package init-net
  :if (locate-library "init-net")
  :demand t)

(use-package init-org
  :if (locate-library "init-org")
  :after viper :defer 1)

(use-package init-comint
  :if (locate-library "init-comint")
  :after comint :defer 0)

(use-package init-rclone
  :if (locate-library "init-rclone")
  :defer 5)

(use-package init-pcmpl
  :if (locate-library "init-pcmpl")
  :after pcomplete :defer 0)

(use-package touch-screen
  :if zr-sys-android-gui-p)

(use-package minibuffer
  :custom
  (enable-recursive-minibuffers t)
  (resize-mini-windows t)
  (history-delete-duplicates t)
  (read-file-name-completion-ignore-case t)
  (completion-styles '(initials partial-completion flex))
  (completion-cycle-threshold 10)
  (completions-max-height 20)
  (completions-detailed t)
  (completions-group t)

  ;; ver 30
  (minibuffer-visible-completions t)
  (minibuffer-regexp-mode t)
  (completions-sort 'historical)
  :hook
  ((emacs-startup . minibuffer-electric-default-mode)
   (emacs-startup . savehist-mode)
   (minibuffer-setup . zr-defer-gc)
   (minibuffer-exit . zr-restore-gc))
  :bind
  ( :map completion-in-region-mode-map
    ("M-n" . minibuffer-next-completion)
    ("M-p" . minibuffer-previous-completion))
  :config
  (advice-add 'completion-at-point :after #'minibuffer-hide-completions))

(use-package icomplete
  :hook
  ((emacs-startup . fido-mode)
   (icomplete-minibuffer-setup
    . (lambda () (kill-local-variable 'completion-styles))))
  :custom
  (icomplete-in-buffer t))

(use-package completion-preview
  :if (package-installed-p 'completion-preview)
  :diminish
  :bind
  ( :map completion-preview-active-mode-map
    ("M-n" . completion-preview-next-candidate)
    ("M-p" . completion-preview-prev-candidate)
    ([remap forward-word] . completion-preview-insert-word)
    ([remap forward-sexp] . completion-preview-insert-sexp))
  :hook (prog-mode inferior-emacs-lisp-mode)
  :config
  (unless zr-sys-winnt-p
    (add-hook 'eshell-mode-hook #'completion-preview-mode)))

(use-package pp
  :bind
  ("M-ESC :" . pp-eval-expression)
  ([remap eval-last-sexp] . pp-eval-last-sexp))

(use-package viper
  :init
  (setq viper-custom-file-name (locate-library "init-viper")
        viper-inhibit-startup-message t
        viper-expert-level 5
        viper-vi-style-in-minibuffer nil
        viper-vi-state-cursor-color nil
        viper-buffer-search-char ?*
        viper-case-fold-search t
        viper-shift-width 2
        viper-auto-indent t
        viper-electric-mode t
        viper-ex-style-motion nil
        viper-ex-style-editing nil
        viper-ESC-moves-cursor-back nil
        viper-mode t)
  :hook (window-setup
         (( calc-embedded-mode
            change-log-mode
            edebug-mode
            org-mode
            org-capture-mode
            log-edit-mode)
          . viper-change-state-to-insert))
  :bind
  ( :map viper-insert-global-user-map
    ("C-t" . viper-exec-key-in-emacs)
    ("C-d" . viper-exec-key-in-emacs)
    ("C-v" . viper-exec-key-in-emacs)
    ("C-w" . viper-exec-key-in-emacs)
    ("C-\\" . viper-exec-key-in-emacs)
    ("<backspace>" . viper-exec-key-in-emacs)
    ("RET" . viper-exec-key-in-emacs))
  ( :map viper-vi-global-user-map
    ("C-u" . viper-exec-key-in-emacs)
    ("C-v" . viper-exec-key-in-emacs)
    ("C-\\" . viper-exec-key-in-emacs)
    ("C-f" . follow-scroll-up)
    ("C-b" . follow-scroll-down)
    :prefix "C-w"
    :prefix-map zr-viper-cw-prefix-map
    ("h" . windmove-left)
    ("l" . windmove-right)
    ("k" . windmove-up)
    ("j" . windmove-down)
    :prefix "g"
    :prefix-map zr-viper-vi-g-prefix-map
    ("g" . beginning-of-buffer)
    ("cc" . comment-line)
    ("t" . tab-line-switch-to-next-tab)
    ("T" . tab-line-switch-to-prev-tab)
    :prefix "SPC"
    :prefix-map zr-viper-vi-spc-prefix-map
    ("d" . duplicate-dwim)
    ("f" . org-open-at-point-global)
    ("L" . org-insert-link-global)
    ("r" . re-builder)
    ("R" . rename-visited-file)
    ("t" . transpose-sentences)
    ("T" . transpose-paragraphs)
    ("p" . viper-prev-destructive-command)
    ("n" . viper-next-destructive-command)
    :prefix "z"
    :prefix-map zr-viper-vi-z-prefix-map
    ("z" . recenter)
    ("T" . transpose-regions))
  ( :map viper-dired-modifier-map
    ("/" . (lambda (&rest args)
             (interactive "P")
             (apply (if (eq major-mode 'wdired-mode)
                        #'viper-exec-key-in-emacs #'viper-search-forward)
                    args)))
    (":" . nil))
  (:repeat-map zr-viper-insert-repeat-map
               ("p" . viper-insert-prev-from-insertion-ring)
               ("n" . viper-insert-next-from-insertion-ring))
  (:repeat-map zr-viper-vi-repeat-map
               ("p" . viper-prev-destructive-command)
               ("n" . viper-next-destructive-command))
  :custom
  (viper-want-ctl-h-help t)
  (viper-no-multiple-ESC nil)
  (ex-cycle-other-window nil)
  (viper-syntax-preference 'emacs)
  :custom-face
  (viper-minibuffer-emacs ((t (:background nil :foreground nil))))
  :config
  (setq viper-vi-state-id nil))

(use-package help
  :custom
  (help-window-select t "Switch to help buffers automatically")
  (help-window-keep-selected t)
  (help-enable-symbol-autoload t)
  (help-enable-variable-value-editing t)
  (three-step-help t)
  (apropos-sort-by-scores t)
  :bind
  (:map help-map
        ("A" . 'apropos-user-option)
        ("V" . 'apropos-value)
        ("D" . 'info-apropos)
        ("M" . 'describe-keymap)
        ("z" . 'shortdoc)
        ("Z" . 'apropos-library))
  :config
  (temp-buffer-resize-mode))

(use-package eldoc
  :custom
  (eldoc-minor-mode-string nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(use-package man
  :custom
  (Man-support-remote-systems t)
  (Man-switches "-a"))

(use-package info
  :custom
  (Info-directory-list (cons (expand-file-name "info" user-emacs-directory)
                             Info-directory-list)))

(use-package bookmark
  :custom
  (bookmark-save-flag 1))

(use-package cua-base
  :hook (emacs-startup . cua-mode)
  :custom
  (cua-enable-cua-keys nil "cua-selection-mode")
  (cua-rectangle-mark-key [(control ~)])
  (cua-prefix-override-inhibit-delay 0.3)
  (cua-delete-selection nil))

(use-package pixel-scroll
  :hook window-setup
  :custom
  (pixel-scroll-precision-large-scroll-height 60)
  (pixel-scroll-precision-interpolation-factor 8.0))

(use-package display-fill-column-indicator
  :unless zr-sys-android-p
  :hook prog-mode
  :custom
  (display-fill-column-indicator-character ?\u254e))

(use-package time :defer 9
  :unless zr-sys-android-p
  :custom
  (display-time-24hr-format t)
  (display-time-use-mail-icon t)
  (display-time-default-load-average nil)
  :config
  (display-time))

(use-package battery :defer 10
  :unless zr-sys-android-p
  :config
  (when (and battery-status-function
             (not (string= "N/A"
                           (battery-format "%B"
                                           (funcall battery-status-function)))))
    (display-battery-mode 1)))

(use-package hl-line
  ;; maybe break table.el based on text-mode
  ;; :hook (org-mode prog-mode comint-mode)
  :hook (emacs-startup . global-hl-line-mode)
  ;; :custom
  ;; (global-hl-line-sticky-flag t)
  )

(use-package display-line-numbers
  :unless zr-sys-android-p
  :custom
  (display-line-numbers-type 'relative))

(use-package subword
  :unless zr-sys-android-p
  :diminish
  :hook prog-mode)

(use-package glasses
  :unless zr-sys-android-p
  ;; :hook prog-mode
  :custom
  (glasses-uncapitalize-p t)
  (glasses-separate-parentheses-p nil))

(use-package re-builder
  :custom
  (reb-re-syntax 'string)
  :bind
  (:repeat-map zr-re-builder-repeat-mode
               ("s" . reb-next-match)
               ("r" . reb-prev-match)))

(use-package isearch
  :custom
  (search-ring-max 26)
  (regexp-search-ring-max 26)
  (isearch-lazy-count t)
  (isearch-allow-motion t)
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
  (isearch-repeat-on-direction-change t))

;; https://github.com/VernonGrant/discovering-emacs/blob/main/show-notes/4-using-whitespace-mode.md
(use-package whitespace
  ;; :hook (emacs-startup . global-whitespace-mode)
  :diminish
  :custom
  (whitespace-style
   '(face empty spaces tabs newline trailing tab-mark newline-mark))
  (whitespace-space-regexp "\\(^ +\\| +$\\)")
  (whitespace-display-mappings
   '((space-mark ?\ [?·] [?.])
     (space-mark ?\  [?¤] [?_])
     (space-mark ?​ [?‸] [?.])
     (newline-mark ?\n [?¬ ?\n] [?$ ?\n])
     (newline-mark ?\r [?¶] [?#])
     (tab-mark ?\t [?» ?\t] [?> ?\t])))
  (whitespace-global-modes
   '(not shell-mode
         help-mode
         magit-mode
         magit-diff-mode
         ibuffer-mode
         dired-mode
         occur-mode))
  (whitespace-action '(report-on-bogus))
  :init
  (put 'whitespace-action 'safe-local-variable 'null))

(use-package files
  :custom
  (safe-local-variable-directories '())
  (save-abbrevs 'silently)
  (kept-new-versions 3)
  (kept-old-versions 1)
  (version-control t)
  (delete-old-versions t)
  (backup-directory-alist
   `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))
  :config
  (dolist (m '(("[^/]\\.dired\\'" . dired-virtual-mode)
               ("/hosts\\'" . conf-mode)))
    (add-to-list 'auto-mode-alist m)))

(use-package tmm
  :custom
  (tmm-shortcut-words nil)
  (tmm-shortcut-inside-entry t)
  (tmm-completion-prompt nil)
  :config
  (if (fboundp 'tmm-add-prompt)
      (advice-add 'tmm-add-prompt :after #'minibuffer-hide-completions)
    (add-to-list 'completion-category-defaults '(tmm (eager-display)))))

(use-package tooltip
  :custom
  (tooltip-mode nil))

(use-package tool-bar
  :if zr-sys-android-gui-p
  :custom
  (tool-bar-button-margin 12)
  (tool-bar-position 'bottom)
  :config
  (modifier-bar-mode))

(use-package imenu
  :custom
  (imenu-flatten 'prefix))

(use-package speedbar
  :init
  (define-key zr-menu [speedbar]
              '(menu-item "speedbar" speedbar))
  :config
  (setopt speedbar-supported-extension-expressions
          (append '(".sql")
                  speedbar-supported-extension-expressions)))

(use-package electric
  :custom
  (electric-layout-rules '())
  :config
  (electric-layout-mode))

(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  (dolist (pair '((?’ . ?‘) (?” . ?“)))
    (add-to-list 'electric-pair-pairs pair))
  (define-advice electric-pair--insert
      (:around (orig-fn c &rest args) fix-curved-quotes)
    "ref: https://emacs-china.org/t/electric-pair/16403/7"
    (if-let* ((qpair (rassoc c electric-pair-pairs))
              ((> (car qpair) (cdr qpair))))
        (run-with-timer 0 nil `(lambda () (backward-char 1) (insert ,c)))
      (apply orig-fn c args))))

(use-package windmove
  :unless zr-sys-android-p
  :hook emacs-startup
  :custom
  (windmove-wrap-around t)
  (windmove-allow-all-windows t))

(use-package server
  :config
  (unless server-process
    (server-start)))

(use-package autorevert
  :custom
  (auto-revert-mode-text " AR")
  (auto-revert-remote-files t))

(use-package diff
  :custom
  (diff-refine-nonmodified t)
  (diff-add-log-use-relative-names t))

(use-package grep
  :custom
  (grep-use-headings t))

(use-package xref
  :init
  (put 'tags-table-list 'safe-local-variable 'listp)
  (put 'xref-etags-mode 'safe-local-eval-function t)
  :custom
  (xref-history-storage 'xref-window-local-history)
  (xref-auto-jump-to-first-xref 'move)
  (tags-revert-without-query t)
  (tags-add-tables nil)
  :config
  (when (string= grep-program "ug")
    (setq xref-search-program 'ugrep)))

(use-package etags-regen
  :if (and (package-installed-p 'etags-regen)
           (not zr-sys-android-p))
  :hook emacs-startup
  :custom
  (etags-regen-tags-file "_tags")
  (etags-regen-regexp-alist
   '((("lisp")
      "/(use-package[ \t]+\\([a-zA-Z0-9\\-]+\\)/usep\\/\\1/")
     (("none")
      "/^\\*+ \\(.+\\)/oh\\/\\1/"
      "/^#\\+name: \\(.+\\)/ob\\/\\1/i")))
  :config
  (let ((options (seq-filter
                  (lambda (s) (string-prefix-p "-" s))
                  (process-lines etags-regen-program "--help"))))
    (dolist (option '("--no-fallback-lang"))
      (when (member option options)
        (add-to-list 'etags-regen-program-options option)))))

(use-package quickurl)

(use-package abbrev
  :diminish '(abbrev-mode . " A")
  :custom
  (abbrev-suggest t))

(use-package skeleton
  :custom
  (skeleton-further-elements '((abbrev-mode nil)))
  (skeleton-pair t))

;; [[info:autotype]]
(use-package auto-insert
  :unless zr-sys-android-p
  :hook emacs-startup
  :custom
  (auto-insert-directory (file-name-concat user-emacs-directory "insert/"))
  :config
  (dolist (template
           '(((makefile-mode . "cmake_launcher") . "cmake.make")
             (("\\.bash\\'" . "trap error") . "trap.bash")))
    (add-to-list 'auto-insert-alist template)))

(use-package auth-source
  :custom
  (auth-source-pass-extra-query-keywords t)
  (auth-source-pass-port-separator "#")
  :config
  (when (executable-find "gopass")
    (auth-source-pass-enable)

    (define-advice auth-source-pass-entries (:around (orig-fn &rest args) symlink)
      "Follow-symlinks."
      (let ((orig-dfr (symbol-function #'directory-files-recursively)))
        (cl-letf (((symbol-function #'directory-files-recursively)
                   (lambda (dir regexp &optional include predicate _follow)
                     (funcall orig-dfr dir regexp include predicate t))))
          (apply orig-fn args))))

    (define-advice auth-source-pass--find-match-many
        (:around (orig-fn hosts users ports &rest args) fix-host-match)
      "Normalize HOSTS."
      (let (norm-hosts url-users url-ports)
        (dolist (h hosts)
          (pcase-let ((`(,nh ,u ,p) (auth-source-pass--disambiguate h)))
            (push nh norm-hosts)
            (when u (push u url-users))
            (when (and p (not (equal "443" p)))
              (push p url-ports))))
        (apply orig-fn norm-hosts (or users url-users) (or ports url-ports) args)))

    (setopt auth-source-pass-filename
            (expand-file-name "gopass/stores/root"
                              (pcase system-type
                                ('windows-nt (getenv "LOCALAPPDATA"))
                                (_ "~/.local/share"))))))

(use-package copyright)
(use-package executable)
(use-package time-stamp)

(use-package happie-exp
  :bind
  ([remap dabbrev-expand] . hippie-expand))

(use-package desktop
  :hook (emacs-startup . desktop-save-mode)
  :custom
  (desktop-restore-frames nil)
  (desktop-restore-eager nil)
  (desktop-auto-save-timeout 600))

;; https://karthinks.com/software/a-consistent-structural-editing-interface/
;; https://karthinks.com/software/persistent-prefix-keymaps-in-emacs/
(use-package repeat
  :hook emacs-startup
  :custom
  (repeat-exit-key "RET")
  (repeat-exit-timeout 10)
  (repeat-echo-function #'repeat-echo-mode-line)
  :bind
  (:repeat-map page-navigation-repeat-map
               :exit
               ("n" . narrow-to-page))
  (:repeat-map comint-repeat-map
               ("n" . comint-next-prompt)
               ("p" . comint-previous-prompt)
               :exit
               ("m" . comint-copy-old-input))
  (:repeat-map other-window-repeat-map
               ("<backspace>" . kill-buffer)
               ("S-<backspace>" . kill-buffer-and-window)
               ("<left>" . windmove-delete-left)
               ("<right>" . windmove-delete-right)
               ("<up>" . windmove-delete-up)
               ("<down>" . windmove-delete-down)
               ("C-<left>" . windmove-swap-states-left)
               ("C-<right>" . windmove-swap-states-right)
               ("C-<up>" . windmove-swap-states-up)
               ("C-<down>" . windmove-swap-states-down)
               ("h" . windmove-display-left)
               ("l" . windmove-display-right)
               ("k" . windmove-display-up)
               ("j" . windmove-display-down)
               ("f" . windmove-display-new-frame)
               ("t" . windmove-display-new-tab)
               ("0" . delete-window)
               ("<" . scroll-left)
               (">" . scroll-right)
               ("v" . scroll-other-window)
               ("V" . scroll-other-window-down)
               :exit
               ("1" . delete-other-windows)
               ("b" . switch-to-buffer))
  :config
  (keymap-unset comint-repeat-map "C-n")
  (keymap-unset comint-repeat-map "C-p")
  )

(use-package elide-head
  :unless zr-sys-android-p
  :hook prog-mode)

;; https://karthinks.com/software/simple-folding-with-hideshow/
(use-package hideshow
  :diminish (hs-minor-mode . nil)
  :hook ((prog-mode . hs-minor-mode)
         ((ediff-prepare-buffer vc-before-checkin) . turn-off-hideshow)))

(use-package paren
  :custom
  (show-paren-predicate '(or (not (derived-mode . special-mode))
                             (major-mode . Info-mode)))
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'child-frame))

(use-package which-func :defer 5
  :autoload which-function
  :unless zr-sys-android-p
  :config
  (which-function-mode))

(use-package midnight :defer 60
  :custom
  (midnight-delay 14400)
  (clean-buffer-list-kill-buffer-names
   '("*Help*" "*Apropos*" "*Completions*"
     "*Ibuffer*" "*Buffer List*" "*buffer-selection*" "*timer-list*"
     "*Compile-Log*" "*Pp Eval Output*" "*Async-native-compile-log*"
     "*Async Shell Command*" "*Shell Command Output*"
     "*Directory*" "*Calculator*" "*Calc Trail*"
     "*vc*" "*vc-diff*" "*diff*" "*xref*"))
  (clean-buffer-list-kill-regexps
   '("\\`\\*Man " "\\`\\*Shortdoc "
     "\\`\\*Newsticker " "\\`\\*newsticker-wget-image-"))
  :config
  (midnight-mode))

(use-package project
  :custom
  (project-vc-extra-root-markers '())
  (project-mode-line t)
  (project-files-relative-names t)
  (project-vc-include-untracked nil)
  (project-kill-buffers-display-buffer-list t))

(use-package editorconfig
  :if (package-installed-p 'editorconfig)
  :after project :defer 1
  :config
  (editorconfig-mode))

(use-package vc
  :custom
  (vc-git-diff-switches '("--textconv"))
  (vc-display-status 'no-backend)
  (vc-handled-backends '(Git SVN))
  (vc-command-messages 'log))

(use-package add-log
  :custom
  (add-log-keep-changes-together t)
  :config
  (modify-syntax-entry ?' "\"" change-log-mode-syntax-table))

(use-package log-edit
  :config
  (modify-syntax-entry ?' "\"" log-edit-mode-syntax-table))

(use-package python
  :custom
  (python-indent-offset 2)
  (python-indent-block-paren-deeper t)
  (python-shell-dedicated t)
  :config
  (when (executable-find "uv")
    (setq python-interpreter "uv"
          python-interpreter-args "run --with isort --with pyflakes python")
    (when zr-sys-winnt-p
      (setq python-shell-prompt-detect-failure-warning nil
            python-shell-interpreter "uv"
            python-shell-interpreter-args "run --with pyreadline3 python -i -X utf8")))
  (modify-syntax-entry ?' "\"" inferior-python-mode-syntax-table))

(use-package flymake
  :hook sh-mode
  :bind
  (:map flymake-mode-map
        ("C-x `" . flymake-goto-next-error))
  (:repeat-map zr-flymake-repeat-map
               ("n" . flymake-goto-next-error)
               ("p" . flymake-goto-prev-error))
  :custom
  (flymake-mode-line-lighter "F")
  (flymake-show-diagnostics-at-end-of-line 'fancy))

(use-package gud
  :custom
  (gud-highlight-current-line t)
  ;; (gud-pdb-command-name "python -X utf8 -m pdb")
  :config
  (modify-syntax-entry ?' "\"" gud-mode-syntax-table))

(use-package bs
  :bind
  ("C-x C-b" . bs-show)
  ("C-x <up>" . bs-cycle-previous)
  ("C-x <down>" . bs-cycle-next)
  (:repeat-map zr-bs-repeat-map
               ("<up>" . bs-cycle-previous)
               ("<down>" . bs-cycle-next))
  :config
  (keymap-set bs-mode-map "i"
              (lambda () (interactive)
                (bs-kill)
                (ibuffer)
                (ibuffer-switch-to-saved-filter-groups "default")))
  (dolist (conf '(("tab-line" nil nil nil
                   (lambda (buf)
                     (let ((gp (funcall tab-line-tabs-buffer-group-function
                                        (current-buffer))))
                       (not (equal gp (funcall tab-line-tabs-buffer-group-function
                                               buf)))))
                   bs--sort-by-name)
                  ("remote" nil nil nil
                   (lambda (b)
                     (not (file-remote-p (buffer-local-value
                                          'default-directory b))))
                   bs--sort-by-name)
                  ("project" nil nil nil
                   (lambda (buf)
                     "ref: `project-buffers'"
                     (let ((root
                            (expand-file-name
                             (file-name-as-directory
                              (project-root (project-current nil))))))
                       (not (string-prefix-p
                             root
                             (expand-file-name
                              (buffer-local-value
                               'default-directory buf))))))
                   bs--sort-by-mode)))
    (add-to-list 'bs-configurations conf t))
  (setq bs-default-configuration "tab-line"
        bs-cycle-configuration-name "project"))

(use-package ibuffer
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

(use-package buff-menu
  :custom
  (Buffer-menu-group-by nil))

(use-package newsticker :defer 5
  :bind
  ( :map newsticker-mode-map
    ("n" . newsticker-next-new-item)
    ("p" . newsticker-previous-new-item)
    ("N" . newsticker-next-item)
    ("P" . newsticker-previous-item))
  :custom
  (newsticker-obsolete-item-max-age 864000)
  (newsticker-treeview-date-format "%y.%m.%d, %H:%M")
  (newsticker-url-list-defaults nil)
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-hide-old-items-in-newsticker-buffer t "plainview only")
  (newsticker-retrieval-interval 1800)
  (newsticker-retrieval-method 'extern)
  (newsticker-wget-name "curl")
  (newsticker-wget-arguments '("-Lkqsm30" "-A" "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Edg/124.0.0.0"))
  :config
  (dolist (map (list newsticker-treeview-mode-map
                     newsticker-treeview-list-mode-map
                     newsticker-treeview-item-mode-map))
    (bind-keys
     :map map
     ("n" . newsticker-treeview-next-new-or-immortal-item)
     ("p" . newsticker-treeview-prev-new-or-immortal-item)
     ("N" . newsticker-treeview-next-item)
     ("P" . newsticker-treeview-prev-item)))
  (define-advice newsticker--treeview-window-init (:before () display-in-new-tab)
    "Display in new tab if not in new frame."
    (unless newsticker-treeview-own-frame
      (tab-bar-new-tab)))
  (define-advice newsticker-treeview-quit (:after () close-created-tab)
    "Close created tab for newsticker."
    (unless (or newsticker-treeview-own-frame
                (> 2 (length (funcall tab-bar-tabs-function))))
      (tab-bar-close-tab)))
  (define-advice newsticker-get-news (:around (orig-fun &rest args) fix-default-directory)
    "Fix issue where `newsticker-get-news' throws an error when the current
directory is deleted.  This bind `default-directory' to `newsticker-dir'
before calling the original function."
    (let ((default-directory newsticker-dir))
      (apply orig-fun args)))

  (defun zr-init-and-start-newsticker (level)
    (interactive "nPrivacy level: ")
    ;; (auth-source-forget-all-cached)
    (load "init-rss.el.gpg" t t)
    (zr-setup-news-url-list level)
    (newsticker-start t)))

(use-package eww
  :bind
  ( :map eww-bookmark-mode-map
    ("n" . next-line)
    ("p" . previous-line)
    ("M-RET" . eww-open-in-new-buffer)
    ("M-n" . eww-next-bookmark)
    ("M-p" . eww-previous-bookmark))
  :custom
  (eww-search-prefix "https://www.mojeek.com/search?newtab=1&cdate=1&qss=DuckDuckGo&date=1&sst=1&arc=none&q=" "https://wiby.org/?q=")
  (eww-auto-rename-buffer 'title)
  (eww-readable-adds-to-history nil)
  (shr-cookie-policy nil)
  (shr-use-xwidgets-for-media t)
  (shr-blocked-images (concat "^https?://" (rx (| "www.baidu.com"))))
  :config
  (setq-mode-local eww-bookmark-mode
                   goal-column (1+ (/ (window-width) 2))))

(use-package webjump
  :init
  (define-key zr-menu [webjump]
              '(menu-item "webjump" webjump))
  :config
  (dolist (web '(("Mojeek" .
                  [simple-query "https://www.mojeek.com"
                                "https://www.mojeek.com/search?newtab=1&cdate=1&qss=Brave,DuckDuckGo,Google,Metager,Swisscows,Yandex,Yep&date=1&sst=1&arc=none&q=" ""])
                 ("Yandex" .
                  [simple-query "https://yandex.com"
                                "https://yandex.com/search/?text=" ""])
                 ("Swisscows" .
                  [simple-query "https://swisscows.com/"
                                "https://swisscows.com/en/web?query=" ""])
                 ("Bing" .
                  [simple-query "https://www.bing.com"
                                "https://www.bing.com/search?q=" ""])
                 ("Bili" .
                  [simple-query "https://www.bilibili.com"
                                "https://search.bilibili.com/all?keyword=" ""])
                 ("NixHome" .
                  [simple-query "https://home-manager-options.extranix.com"
                                "https://home-manager-options.extranix.com/?query=" ""])
                 ("NixPackage" .
                  [simple-query "https://search.nixos.org/packages"
                                "https://search.nixos.org/packages?from=0&size=50&sort=relevance&type=packages&query=" ""])
                 ("NixOption" .
                  [simple-query "https://search.nixos.org/options"
                                "https://search.nixos.org/options?from=0&size=50&sort=relevance&type=packages&query=" ""])))
    (add-to-list 'webjump-sites web)))

(use-package browse-url
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode))
  :custom
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser))))

(use-package tramp
  :init
  (defvar zr-tramp-menu (make-sparse-keymap "tramp"))
  (define-key zr-menu [tramp-menu]
              (list 'menu-item "tramp-menu" zr-tramp-menu))
  (define-key zr-tramp-menu [tramp-cleanup-connection]
              '(menu-item "cleanup-connection" tramp-cleanup-connection))
  (define-key zr-tramp-menu [tramp-cleanup-bufferless-connections]
              '(menu-item "cleanup-bufferless-connections"
                          tramp-cleanup-bufferless-connections))
  (define-key zr-tramp-menu [tramp-cleanup-some-buffers]
              '(menu-item "cleanup-some-buffers" tramp-cleanup-some-buffers))
  :custom
  (tramp-verbose 0)
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-locks t)
  (remote-file-name-inhibit-auto-save t)
  (remote-file-name-inhibit-auto-save-visited t)
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  (tramp-use-connection-share t)
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process))

(use-package nxml-mode
  :mode "\\.wsb\\'")

(use-package cc-mode
  :config
  (define-abbrev-table 'c++-mode-abbrev-table
    '(("mun" "[[maybe_unused]]")
      ("fall" "[[fallthrough]]"))))

(use-package sh-script
  :custom
  (sh-basic-offset 2)
  (sh-shellcheck-arguments '("-e" "SC1017"))
  :config
  (setq-mode-local sh-base-mode
                   buffer-file-coding-system 'prefer-utf-8-unix))

(use-package shell
  :custom
  (shell-completion-execonly nil)
  (shell-completion-fignore '("~" "#" "%"))
  (shell-get-old-input-include-continuation-lines t)
  (shell-kill-buffer-on-exit t)
  :config
  (modify-syntax-entry ?' "\"" shell-mode-syntax-table))

(use-package sql
  :bind
  ( :map sql-mode-map
    ("C-c C-p" . sql-connect)
    :prefix "C-c C-k"
    :prefix-map zr-sql-cc-ck-prefix-map
    ("a" . sql-list-all)
    ("t" . sql-list-table))
  ( :map sql-interactive-mode-map
    :prefix "C-c"
    :prefix-map zr-sqli-cc-prefix-map
    ("C-y" . sql-copy-column)
    ("C-w" . backward-kill-word)
    ("C-l" . comint-dynamic-list-input-ring)
    :prefix "C-c C-k"
    :prefix-map zr-sql-cc-ck-prefix-map)
  :hook
  (((sql-mode sql-interactive-mode)
    . (lambda ()
        (setq-local comment-start "-- ")))
   ((sql-mode sql-interactive-mode) . sql-indent-enable))
  :custom
  (sql-input-ring-file-name (locate-user-emacs-file "sql-history.eld")))

(use-package js
  :custom
  (js-indent-level 4)
  :init
  (with-eval-after-load 'autoinsert
    (add-to-list 'auto-insert-alist
                 '(("\\.js\\'" . "strict mode")
                   nil "\"use strict\"" n "(() => {" n > _ 10 "})();"))))

(use-package json-ts-mode
  :custom
  (json-ts-mode-indent-offset 4))

(use-package dired
  :custom
  (dired-movement-style 'cycle)
  (dired-maybe-use-globstar t)
  (dired-dwim-target t)
  (dired-use-ls-dired t)
  (dired-listing-switches "-lHogvFh")
  (dired-ls-F-marks-symlinks t)
  (dired-mouse-drag-files t)
  (delete-by-moving-to-trash t)
  (dired-hide-details-hide-absolute-location t)
  (dired-guess-shell-alist-user
   `(("\\.cb7\\'"
      (format "ebook-convert ? %s --no-process"
              (shell-quote-argument (file-name-with-extension file "zip"))))
     (,(rx ?. (| "rar" "zip" "7z" "iso" "cab" "apks" "cb7" "cbz" "cbr") (? ".001") eos)
      #1=(format "%s x -spe -o\"%s\" -aou -p\"%s\""
                 archive-7z-program (file-name-sans-extension file)
                 (if zr-sys-winnt-p zr-archive-pwd
                   (format "$(printf '%s' | iconv -f utf-8 -t gbk)"
                           zr-archive-pwd))))
     ("\\.docx?\\'"
      (format "pandoc -o \"%s.org\""
              (file-name-sans-extension file)))
     ("\\.exe\\'" "innounp -x -q -o -b -u -pixyg688.com")
     ("\\.apk\\'"
      (format "adb %sinstall"
              (let ((devices
                     (mapcar
                      (lambda (a) (replace-regexp-in-string
                               "[[:blank:]]+device$" "" a))
                      (cl-delete-if-not
                       (lambda (a) (string-suffix-p "device" a))
                       (process-lines "adb" "devices")))))
                (if (> 2 (length devices)) ""
                  (format "-s \"%s\" "
                          (completing-read "Device: " devices))))))
     ("\\.apk\\'"
      (format "%s sign --ks \"%s\" --ks-pass \"pass:emacs1\" --ks-key-alias \"Emacs keystore\""
              (if-let* ((base-name "apksigner")
                        (apksigner (executable-find base-name)))
                  apksigner
                (if-let* ((adb (executable-find "adb"))
                          (build-path (locate-dominating-file adb "build-tools"))
                          (exe (car (directory-files-recursively
                                     build-path
                                     (format "^%s%s$" base-name
                                             (regexp-opt exec-suffixes))))))
                    exe
                  base-name))
              zr-emacs-keystore-file))
     (,(rx ?. (| "tzst" "tar.zst") eos)
      "zstd -dc ? | tar -xf -")
     (,(rx ?. (| "srt" "ass") eos)
      (apply #'format
             "ffmpeg -hide_banner -itsoffset 0 -i ? -i %s -c copy -disposition:s:0 default -c:s mov_text %s"
             (mapcar #'shell-quote-argument
                     (if-let* ((f (car kill-ring))
                               ((file-exists-p f)))
                         (list f (concat "ff-" (file-name-nondirectory f)))
                       (make-vector 2 (file-name-with-extension file "mp4"))))))
     (,(rx ?. (| "mp4" "mkv" "avi" "webm" "flv" "m4v" "mov") eos)
      ;; https://docs.nvidia.com/video-technologies/video-codec-sdk/12.0/ffmpeg-with-nvidia-gpu/index.html#command-line-for-latency-tolerant-high-quality-transcoding
      "ffmpeg -hide_banner -y -i ? -c:a libopus -b:a 128k -c:v hevc_nvenc -vf \"scale='min(2560,iw):-1\" -rc vbr -cq 28 -b:v 0 -bufsize 15M -maxrate 15M -g 250 -bf 3 -b_ref_mode 2 -temporal-aq 1 -rc-lookahead 20 -i_qfactor 0.75 -b_qfactor 1.1 -fps_mode passthrough ff-`?`")
     (,(rx ?. (| "png" "jpeg" "jpg" "gif" "webp" "bmp") eos)
      "ffmpeg -hide_banner -y -i ? -vf \"scale='min(4096,iw)':-1\" -c:v libaom-av1 -cpu-used 6 -row-mt 1 -tiles 2x2 -still-picture 1 -crf 20 -f avif ff-`?`")
     (".*" (format "tar -cf - ? | zstd -o %s --long --ultra -9 ;"
                   (if (file-directory-p file) "`?`.tzst"
                     (shell-quote-argument
                      (file-name-with-extension file "tzst")))))
     (".*" (format "ls -lSR ? > %s ;"
                   (if (file-directory-p file) "`?`.dired"
                     (shell-quote-argument
                      (file-name-with-extension file "dired")))))
     (".*" #1#)))
  (wdired-allow-to-change-permissions 'advanced)
  (wdired-use-interactive-rename t)
  :bind
  ( :map dired-mode-map
    ("<mouse-2>" . dired-mouse-find-file)
    ("C-p" . viper-previous-line)
    ("C-n" . viper-next-line)
    :prefix "SPC"
    :prefix-map zr-dired-spc-prefix-map
    ("a" . org-attach-dired-to-subtree))
  :config
  (defvar zr-archive-pwd "⑨"
    "Default archive passwd.")
  (when-let* ((7z (or (executable-find "7z")
                     (executable-find "7zz")
                     (executable-find "7za"))))
    (setq archive-7z-program (file-name-base 7z))))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep 't)
  (dired-vc-rename-file t)
  (dired-isearch-filenames 'dwim)
  (dired-compress-file-default-suffix ".zst")
  (dired-compress-directory-default-suffix ".tar.zst")
  :config
  (dolist (item `(("\\.\\(tar\\.zst\\)\\'" . "tar -cf - %i | zstd -T0 --fast=2 -o %o")
                  ("\\.7z\\'" . ,(format "%s a -mqs=on -mx3 %%o %%i" archive-7z-program))
                  ("\\.zip\\'" . ,(format "%s a -mx3 %%o %%i" archive-7z-program))))
    (add-to-list 'dired-compress-files-alist item))

  (dolist (item `(("\\.\\(zip\\|rar\\)\\'" #1=""
                   ,(format "%s x -aoa -o%%o %%i" archive-7z-program))
                  ("\\.t\\(ar\\.\\)?\\(gz\\|xz\\|bz\\)\\'" #1#
                   ,(format "%s x %%i -so | %s x -aoa -si -ttar -o%%o"
                            archive-7z-program archive-7z-program ))
                  ("\\.t\\(ar\\.\\)?zst\\'" #1# "zstd -dc %i | tar -xf -")
                  ("\\.zst\\'" #1# "zstd -d --rm %i")))
    (add-to-list 'dired-compress-file-suffixes item)))

(use-package dired-x :defer 1
  :after dired
  :autoload dired-virtual-mode
  :custom
  (dired-find-subdir t)
  (dired-x-hands-off-my-keys nil))

(use-package arc-mode
  :config
  ;; use 7z manipulate rar archive
  (advice-add 'archive-rar-summarize :before-until #'archive-7z-summarize)
  (advice-add 'archive-rar-extract :before-until #'archive-7z-extract))

(use-package find-dired
  :config
  (unless zr-sys-winnt-p
    (setq find-ls-option '("-exec ls -ldh {} +" . "-ldh"))))

(use-package hexl)

(use-package dictionary
  :init
  (define-key zr-menu [dictionary-search]
              '(menu-item "dictionary-search" dictionary-search))
  :custom
  (dictionary-server "dict.tw")
  (dictionary-use-single-buffer t))

(use-package epg
  :config
  (unless zr-sys-linux-p
    (setq epg-pinentry-mode 'loopback)))

(use-package plstore
  :init
  (defcustom zr-local-pls nil
    "Temp local plstore."
    :safe t
    :local 'permanent-only))

(use-package mpc
  :custom
  (mpc-host "127.0.0.1"))

(use-package avoid :defer 15
  :unless zr-sys-android-p
  :config
  (mouse-avoidance-mode 'exile))

(use-package chart
  :commands
  (chart-file-count
   chart-space-usage
   chart-emacs-storage
   chart-emacs-lists
   chart-rmail-from))

;; https://karthinks.com/software/different-strokes-for-different-folks/
(use-package strokes
  :unless zr-sys-android-p
  :custom
  (strokes-lighter nil)
  :bind ("<down-mouse-3>" . 'strokes-do-stroke))

(use-package master
  :bind
  (:repeat-map zr-master-repeat-map
               ("n" . master-says-scroll-up)
               ("p" . master-says-scroll-down)
               ("l" . master-says-recenter)
               ("<" . master-says-beginning-of-buffer)
               (">" . master-says-end-of-buffer)))

(use-package scroll-all
  :config
  (define-advice scroll-all-check-to-scroll
      (:before-while () support-other-scroll-command)
    "Cua.."
    (pcase this-command
      ('cua-scroll-up (call-interactively 'scroll-all-page-down-all))
      ('cua-scroll-down (call-interactively 'scroll-all-page-up-all))
      (_ t))))

(use-package follow
  :custom
  (follow-mode-line-text " Fl")
  :bind
  ( :repeat-map zr-follow-repeat-map
    ("n" . follow-next-window)
    ("p" . follow-previous-window)
    ("l" . follow-recenter)))

(use-package ispell
  :custom
  (ispell-personal-dictionary (locate-user-emacs-file "_dict.txt")))

(use-package flyspell
  :hook (text-mode
         (prog-mode . flyspell-prog-mode))
  :bind
  ( :map flyspell-mode-map
    ("C-." . nil)
    ("C-," . nil)
    ("C-c $" . nil)
    ("C-'" . flyspell-auto-correct-word)
    ("C-\\" . flyspell-goto-next-error))
  :custom
  (flyspell-mode-line-string nil)
  (flyspell-use-meta-tab nil)
  (flyspell-issue-message-flag nil)
  (ispell-personal-dictionary (expand-file-name "dict.txt" user-emacs-directory)))

(use-package calendar
  :custom
  (diary-file (locate-user-emacs-file "_diary"))
  (calendar-date-style 'iso)
  (calendar-latitude 30.6)
  (calendar-longitude 114.3)
  (calendar-chinese-all-holidays-flag t))

(use-package todo-mode
  :init
  (define-key zr-menu [todo-show]
              '(menu-item "todo-show" todo-show))
  :custom
  (todo-directory (locate-user-emacs-file "_todo/")))

(use-package diary-lib
  :custom
  (diary-number-of-entries 7)
  :init
  (add-hook 'diary-list-entries-hook #'diary-sort-entries 50)
  :hook
  ((diary-list-entries . diary-include-other-diary-files)
   (diary-mark-entries . diary-mark-included-diary-files)))

(use-package image
  :bind
  ( :map image-map
    ("k" . image-kill-buffer))
  :custom
  (image-dired-external-viewer "ffplay -fs -an -noborder")
  (image-use-external-converter t)
  (doc-view-scale-internally nil)
  (doc-view-resolution 300)
  :config
  (add-to-list 'image-file-name-extensions "avif"))

(use-package eglot
  ;; :bind
  ;; ( :map eglot-mode-map
  ;;   ([remap indent-region] . eglot-format))
  :custom
  (eglot-menu-string "")
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.1)
  (eglot-events-buffer-config '(:size 0 :format full))
  :config
  ;; (when (executable-find "deno")
  ;;   ;; https://docs.deno.com/runtime/getting_started/setup_your_environment/#eglot
  ;;   ;; https://docs.deno.com/runtime/reference/lsp_integration/#language-ids
  ;;   (add-to-list 'eglot-server-programs
  ;;                '(((js-mode :language-id "javascript")
  ;;                   (js-ts-mode :language-id "javascript")
  ;;                   (tsx-ts-mode :language-id "typescriptreact")
  ;;                   (typescript-ts-mode :language-id "typescript")
  ;;                   (js-json-mode :language-id "json")
  ;;                   (json-ts-mode :language-id "json")
  ;;                   (markdown-ts-mode :language-id "markdown"))
  ;;                  . ("deno" "lsp"
  ;;                     :initializationOptions
  ;;                     ( :enable t
  ;;                       :codeLens.implementations t
  ;;                       :codeLens.references t
  ;;                       :codeLens.referencesAllFunctions t
  ;;                       :codeLens.test t
  ;;                       :suggest.completeFunctionCalls t
  ;;                       :suggest.names t
  ;;                       :suggest.paths t
  ;;                       :suggest.autoImports t
  ;;                       :suggest.imports.autoDiscover t
  ;;                       :suggest.imports.hosts t
  ;;                       :lint t
  ;;                       :unstable t)))))
  (add-hook 'eglot-managed-mode-hook
            (lambda () (setq-local project-mode-line (not (eglot-managed-p))))))

(use-package smerge-mode
  :bind
  (:repeat-map zr-smerge-repeat-map
               ("n" . smerge-next)
               ("p" . smerge-prev)
               ("r" . smerge-resolve)
               ("a" . smerge-keep-all)
               ("b" . smerge-keep-base)
               ("l" . smerge-keep-lower)
               ("u" . smerge-keep-upper)
               ("m" . smerge-keep-current)
               ("E" . smerge-ediff)
               ("C" . smerge-combine-with-next)
               ("R" . smerge-refine)
               ("<" . smerge-diff-base-upper)
               (">" . smerge-diff-base-lower)
               ("=" . smerge-diff-upper-lower)))

(use-package ediff
  :custom
  (ediff-use-last-dir t)
  (ediff-use-long-help-message t)
  (ediff-show-clashes-only t)
  (ediff-make-buffers-readonly-at-startup t)
  (ediff-keep-variants nil)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  :config
  (unless zr-sys-android-gui-p
    (setq ediff-split-window-function 'split-window-horizontally))
  (add-hook 'ediff-before-setup-hook #'tab-bar-new-tab)
  ;; close tab after 'ediff-cleanup-mess
  (add-hook 'ediff-quit-hook #'tab-bar-close-tab 1))

(use-package smtpmail
  :custom
  (smtpmail-stream-type 'starttls)
  (smtpmail-store-queue-variables t))

(use-package message
  :hook (message-send . ispell-message)
  :custom
  (message-server-alist
   '(((lambda () (zr-message-from-matches-p "@\\(qq\\|foxmail\\)\\.com\\'" t))
      . "smtp smtp.qq.com 587")))
  (message-kill-buffer-on-exit t)
  (message-confirm-send t)
  (message-signature nil)
  :config
  (defun zr-message-from-matches-p (regexp &optional set-user-p)
    (when-let* ((from (cadr (mail-extract-address-components
                             (message-fetch-field "From"))))
                ((string-match-p regexp from)))
      (if set-user-p
          (setq smtpmail-smtp-user from)
        t))))

(use-package rmail
  :custom
  (rmail-remote-password-required t))

(use-package gnus
  :hook ((gnus-select-group . gnus-group-set-timestamp)
         (gnus-summary-exit . gnus-summary-bubble-group)
         (gnus-before-startup . (lambda ()
                                  (tab-bar-history-back)
                                  (tab-bar-switch-to-tab "*Gnus*")))
         (gnus-before-resume . (lambda () (tab-bar-switch-to-tab "*Gnus*")))
         (gnus-after-exiting-gnus . tab-bar-close-tab)
         (gnus-configure-windows . gnus-tree-perhaps-minimize))
  :custom
  (gnus-home-directory (expand-file-name "gnus/" user-emacs-directory))
  (gnus-directory (expand-file-name "News/" gnus-home-directory))
  (gnus-init-file (expand-file-name ".gnus.gpg" gnus-home-directory))
  (gnus-sieve-file (expand-file-name ".sieve" gnus-home-directory))
  (gnus-use-full-window nil)
  (gnus-save-newsrc-file nil)
  (gnus-read-newsrc-file nil)
  (gnus-use-trees t)
  (gnus-asynchronous t)
  (gnus-thread-sort-functions '((not gnus-thread-sort-by-number)
                                gnus-thread-sort-by-score))
  (gnus-refer-article-method '(current (nnweb "google" (nnweb-type google))))
  (gnus-single-article-buffer t)
  (gnus-message-archive-group nil)
  (gnus-suppress-duplicates t)
  (gnus-blocked-images "ads")
  (nnmail-expiry-wait 'never)
  (nnmail-expiry-target "Deleted Messages")
  (mm-inline-large-images t)
  (mm-file-name-rewrite-functions
   '(mm-file-name-trim-whitespace
     mm-file-name-collapse-whitespace
     mm-file-name-replace-whitespace))
  :config
  (autoload 'gnus-tree-perhaps-minimize "gnus-salt")
  (auto-image-file-mode)
  (gnus-delay-initialize))

(use-package remember
  :custom
  (remember-diary-file (file-name-concat org-directory "remember"))
  :custom
  (add-to-list 'remember-handler-functions 'remember-diary-extract-entries))

(use-package proced
  :init
  (define-key zr-menu [proced]
              '(menu-item "proced" proced))
  :custom
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t))

(use-package pcomplete
  :custom
  (pcomplete-autolist t)
  (pcomplete-recexact t)
  (pcomplete-termination-string ""))

(use-package forms)
(use-package ses)

(use-package tab-bar
  :hook emacs-startup
  :bind
  (:repeat-map tab-bar-move-repeat-map
               ("G" . tab-group)
               ("0" . tab-close)
               ("r" . tab-rename)
               ("u" . tab-undo))
  (:repeat-map zr-tab-bar-history-repeat-map
               ("<right>" . tab-bar-history-forward)
               ("<left>" . tab-bar-history-back))
  :custom
  (tab-bar-format '(tab-bar-format-tabs-groups))
  (tab-bar-define-keys 'numeric)
  (tab-bar-select-tab-modifiers '(control))
  (tab-bar-tab-hints t)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-history-limit 100)
  :custom-face
  (tab-bar ((t (:inherit mode-line :box nil))))
  (tab-bar-tab ((t (:inherit mode-line :box t))))
  (tab-bar-tab-inactive ((t (:inherit mode-line-inactive :box nil))))
  :config
  (defun zr-tab-bar-format-setup ()
    (if (eq t (framep (selected-frame)))
        (setopt tab-bar-show 1
                tab-bar-format
                (remove 'tab-bar-format-global tab-bar-format))
      (add-to-list 'tab-bar-format 'tab-bar-format-global)
      (setopt tab-bar-show t)))
  (add-hook 'server-after-make-frame-hook #'zr-tab-bar-format-setup)
  (unless zr-sys-android-gui-p
    (setq tab-bar-close-button-show nil))
  (tab-bar-history-mode))

(use-package tab-line
  :hook ((emacs-startup . global-tab-line-mode)
         (global-tab-line-mode
          . (lambda ()
              (if global-tab-line-mode
                  (delete 'mode-line-buffer-identification
                          mode-line-format)
                (setcdr (cdddr mode-line-format)
                        (cons 'mode-line-buffer-identification
                              (cddddr mode-line-format)))))))
  :init
  (when (version< emacs-version "30")
    (defvar-keymap tab-line-mode-map
      :doc "Keymap for keys of `tab-line-mode'."))
  :bind
  (:map tab-line-mode-map
        ("C-<tab>" . tab-line-switch-to-next-tab)
        ("C-S-<tab>" . tab-line-switch-to-prev-tab)
        ("C-S-<iso-lefttab>" . tab-line-switch-to-prev-tab)
        ("C-x C-<left>" . nil)
        ("C-x C-<right>" . nil)
        ("C-x <left>" . nil)
        ("C-x <right>" . nil))
  :custom
  (tab-line-tabs-function 'tab-line-tabs-buffer-groups)
  (tab-line-exclude-modes
   '(completion-list-mode
     reb-mode
     special-mode
     gnus-server-mode
     gnus-group-mode
     gnus-summary-mode
     gnus-tree-mode
     gnus-article-mode
     calendar-mode
     calc-mode
     calc-trail-mode
     newsticker-treeview-item-mode
     newsticker-treeview-list-mode
     newsticker-treeview-mode))
  :config
  (unless zr-sys-android-gui-p
    (setq tab-line-new-button-show nil
          tab-line-close-button-show nil)))

(use-package mouse
  :config
  (dolist (gp `((,(rx bos (| "news" "dictionary" "shortdoc") eos) . "Help")
                (,(rx (| (: "shell" eos) "IELM")) . "repl")
                (,(rx bos "sql") . "SQL")
                (,(rx bos "Dired") . "Dired")
                (,(rx (| (: "buffer" (| ?  "-selection-") "menu") "ibuffer") eos) . "BS")))
    (add-to-list 'mouse-buffer-menu-mode-groups gp))
  (setq tab-line-tabs-buffer-groups mouse-buffer-menu-mode-groups))

(use-package window
  :custom
  (even-window-sizes nil)
  (fit-window-to-buffer-horizontally t)
  (window-sides-slots '(0 2 1 2))
  (switch-to-buffer-in-dedicated-window 'pop)
  ;; (display-buffer-alist
  ;;  `(("^\\*eldoc for"
  ;;     #1=(display-buffer-reuse-window
  ;;         display-buffer-below-selected)
  ;;     #11=(window-height . shrink-window-if-larger-than-buffer))
  ;;    ((or (major-mode . bs-mode)
  ;;         "^\\*Buffer List\\*\\'")
  ;;     #1# #11#)
  ;;    ((or ,(regexp-quote shell-command-buffer-name)
  ;;         ,(regexp-quote shell-command-buffer-name-async)
  ;;         ,(regexp-quote shell-command-default-error-buffer))
  ;;     (display-buffer-reuse-window
  ;;      display-buffer-reuse-mode-window
  ;;      display-buffer-in-previous-window
  ;;      display-buffer-below-selected)
  ;;     (mode . (fundamental-mode shell-mode))
  ;;     #11# #21=(inhibit-same-window . nil)
  ;;     #41=(window-parameters . ((no-other-window . t))))
  ;;    ((major-mode . completion-list-mode)
  ;;     #3=display-buffer-at-bottom #11#)))
  ;; :config
  ;; (if zr-sys-android-gui-p
  ;;     (setq window-sides-slots '(0 0 0 0))
  ;;   (dolist (v `(((or (major-mode . Info-mode)
  ;;                     (major-mode . help-mode)
  ;;                     (major-mode . apropos-mode)
  ;;                     (major-mode . emacs-news-mode)
  ;;                     (major-mode . emacs-news-view-mode)
  ;;                     ,(rx bos ?* (| (: (? "Wo") "Man") "Dictionary" "Shortdoc")))
  ;;                 #2=display-buffer-in-side-window
  ;;                 (side . right)
  ;;                 #41=(window-parameters . ((no-other-window . t)))
  ;;                 #21=(window-width . 80))
  ;;                ((or (major-mode . proced-mode)
  ;;                     (major-mode . inferior-emacs-lisp-mode)
  ;;                     ,(rx (| "diff" "xref" "grep" "Occur") ?* eos))
  ;;                 (display-buffer-reuse-mode-window
  ;;                  display-buffer-in-previous-window
  ;;                  display-buffer-in-direction)
  ;;                 #21# (direction . leftmost)
  ;;                 (mode . ( proced-mode xref--xref-buffer-mode
  ;;                           diff-mode grep-mode occur-mode))
  ;;                 #31=(inhibit-same-window . nil))
  ;;                ("\\e?shell\\*\\'"
  ;;                 #2# (side . top)
  ;;                 #12=(window-height . 0.5))))
  ;;     (add-to-list 'display-buffer-alist v)))
  )

(use-package saveplace :hook (emacs-startup . save-place-mode))

(use-package recentf
  :hook emacs-startup
  :init
  (define-key zr-menu [recentf]
              '(menu-item "recentf" recentf))
  :config
  (recentf-mode)
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/data/data/com\\.termux/files/usr/tmp" "/tmp/" "/ssh:"
                     "/sshx:" ,(file-name-concat package-user-dir ".*-autoloads\\.el\\'"))))

(use-package term/xterm
  :custom
  (xterm-extra-capabilities '(modifyOtherKeys setSelection)))

(use-package esh-mode
  :init
  (define-key zr-menu [eshell]
              '(menu-item "eshell" eshell))
  :bind
  ( :map eshell-proc-mode-map
    ;; Kill eshell buffer if no process, like `comint-send-eof'
    ("C-c C-d" . (lambda () (interactive)
                   (or (eshell-send-eof-to-process)
                       (kill-current-buffer)))))
  ( :repeat-map eshell-command-repeat-map
    ("C-b" . nil)
    ("C-f" . nil)
    ("b" . eshell-backward-argument)
    ("f" . eshell-forward-argument))
  ( :repeat-map eshell-prompt-repeat-map
    ("C-n" . nil)
    ("C-p" . nil)
    ("n" . eshell-next-prompt)
    ("p" . eshell-previous-prompt)
    :exit
    ("m" . eshell-copy-old-input))
  :custom
  (eshell-aliases-file nil)
  (eshell-pushd-dunique t)
  (eshell-pushd-dextract t)
  (eshell-scroll-to-bottom-on-output 'others)
  (eshell-history-size 1000)
  (eshell-hist-ignoredups 'erase)
  (eshell-history-isearch 'dwim)
  (eshell-visual-options '(("nix" "--help")
                           ("nixos-rebuild" "--help")))
  :config
  (require 'init-esh)
  (modify-syntax-entry ?' "\"" eshell-mode-syntax-table)
  (add-hook 'eshell-expand-input-functions #'eshell-expand-history-references)
  (dolist (mod '(eshell-smart eshell-elecslash eshell-tramp eshell-xtra))
    (add-to-list 'eshell-modules-list mod)))

(use-package comint
  :bind
  (:map comint-mode-map
        ("SPC" . comint-magic-space))
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

(use-package compile
  :custom
  (compilation-environment '("TERM=xterm-256color"))
  (ansi-osc-for-compilation-buffer t)
  :config
  (unless zr-sys-winnt-p
    (with-eval-after-load 'tramp
      (remove-hook 'compilation-mode-hook
                   #'tramp-compile-disable-ssh-controlmaster-options)))
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-filter-hook #'ansi-osc-compilation-filter))

(use-package gdb-mi
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package outline
  :custom
  (outline-minor-mode-use-buttons 'in-margins)
  (outline-minor-mode-cycle t)
  :config
  (require 'foldout))

(use-package url
  :custom
  (url-privacy-level 'high)
  (url-mime-language-string "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6")
  (url-cookie-trusted-urls '())
  (url-cookie-untrusted-urls '(".*"))
  (url-gateway-local-host-regexp (rx (| (group (| ?. bos)
                                               (| "mshome.net"
                                                  "localhost"
                                                  "local"))
                                        (group bos (| "127.0.0.1")))
                                     eos)))

(use-package filesets :defer 10
  :unless zr-sys-android-p
  :config
  (filesets-init))

(use-package filecache
  :config
  (setq file-cache-alist '(("aria2.conf" "~/.config/aria2/")
                           ("init.el" "~/.emacs.d/")
                           ("default.el" "~/.emacs.d/lisp/")
                           ("pip.ini" "~/.config/pip/")
                           ("config.txt" "~/.config/yt-dlp/"))))

(use-package shadowfile :defer 11
  :unless zr-sys-android-p
  :config
  (shadow-initialize)
  (with-eval-after-load 'project
    (cl-delete-if
     (lambda (a) (member a '(buffer-file-name
                         (and
                          (major-mode . fundamental-mode)
                          "\\`[^ ]"))))
     project-kill-buffer-conditions)
    (dolist (k '((and buffer-file-name
                      (not (and
                            (major-mode . fundamental-mode)
                            #1="\\`\\(?:shadow\\(?:_todo\\|s\\)\\)\\'")))
                 (and
                  (major-mode . fundamental-mode)
                  "\\`[^ ]"
                  (not #1#))))
      (add-to-list 'project-kill-buffer-conditions k))))

(use-package treesit :defer 1
  :after prog-mode
  :custom (treesit-font-lock-level 4)
  :config
  (pcase system-type
    ((and 'windows-nt
          (let ts-extra (substitute-in-file-name
                         "$USERPROFILE/scoop/apps/treesit-langs/current"))
          (guard (file-directory-p ts-extra)))
     (add-to-list 'treesit-extra-load-path ts-extra)))
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (lua "https://github.com/MunifTanjim/tree-sitter-lua")
          (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
          (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          ;; ini
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (nix "https://github.com/nix-community/tree-sitter-nix")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          ;; frontend
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
          ;; doc
          (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")
          (markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))))
  (defun zr-treesit-install-all ()
    "Install all Tree-sitter grammars defined in `treesit-language-source-alist`.
With prefix argument FORCE, delete the tree-sitter directory first.
ref:
https://www.masteringemacs.org/article/how-to-get-started-tree-sitter"
    (interactive "P")
    (dolist (g treesit-language-source-alist)
      (unless (treesit-language-available-p (car g))
        (treesit-install-language-grammar (car g))))))

(use-package markdown-ts-mode
  :if (treesit-language-available-p 'markdown)
  :mode "\\.md\\'")

(use-package cmake-ts-mode
  :if (treesit-language-available-p 'cmake)
  :mode "\\(?:CMakeLists\\.txt\\|\\.cmake\\)\\'"
  :init
  ;; https://www.internalpointers.com/post/modern-cmake-beginner-introduction
  ;; https://cliutils.gitlab.io/modern-cmake/README.html
  (with-eval-after-load 'autoinsert
    (add-to-list 'auto-insert-alist
                 '("CMakeLists\\.txt\\'" "Project: "
                   "cmake_minimum_required(VERSION 3.20)" n
                   "project(" str n
                   "        VERSION 0.1" n
                   "        LANGUAGES "
                   (setq v1 (completing-read "language: " '("CXX" "C"))) n
                   ")" n n
                   "set(CMAKE_EXPORT_COMPILE_COMMANDS ON)" n n
                   "add_executable(" str ?\  _ ")" n n
                   (when (string= v1 "CXX")
                     (setq v1 "default_compiler_flags"
                           v2 "cxx_std_23")
                     (insert
                      "add_library(" v1 " INTERFACE)\n"
                      "set(gcc_like_cxx \"$<COMPILE_LANG_AND_ID:CXX,ARMClang,AppleClang,Clang,GNU,LCC>\")\n"
                      "set(msvc_cxx \"$<COMPILE_LANG_AND_ID:CXX,MSVC>\")\n"
                      "target_compile_features(" v1 " INTERFACE " v2 ")\n"
                      "target_compile_options(" v1 " INTERFACE\n"
                      "  \"$<${gcc_like_cxx}:$<BUILD_INTERFACE:-Wall;-Wextra;-Wshadow;-Wformat=2;-Wunused>>\"\n"
                      "  \"$<${msvc_cxx}:$<BUILD_INTERFACE:-W3>>\"\n"
                      ")\n\n"
                      "target_compile_features(" str " PRIVATE " v2 ")\n"
                      "target_link_libraries(" str " PUBLIC " v1 ")"))
                   n))))

(use-package custom
  :custom
  (custom-buffer-done-kill t))

(use-package table
  :custom-face
  (table-cell ((t (:inherit highlight :foreground nil :background nil)))))

(use-package calc
  :config
  (setq math-additional-units
        '((bit "b" "bit")
          (byte "8 b" "byte")
          (KB "1024 byte" "KB")
          (MB "1024 KB" "MB")
          (GB "1024 MB" "GB")
          (TB "1024 GB" "TB")
          (PB "1024 TB" "PB")
          (EB "1024 PB" "EB")
          (kb "1000 byte" "kb")
          (mb "1000 kb" "mb")
          (gb "1000 mb" "gb")
          (tb "1000 gb" "tb")
          (pb "1000 tb" "pb")
          (eb "1000 pb" "eb"))
        math-units-table nil
        calc-display-trail nil))

(use-package appt :defer 2
  :custom
  (appt-message-warning-time 60)
  (appt-display-interval 5)
  :config
  (appt-activate 1))

(use-package org
  :init
  (setq org-directory "~/org")
  (put 'org-reverse-note-order 'safe-local-variable 'booleanp)
  (defvar zr-org-menu (make-sparse-keymap "org-menu")
    "My useful org menu in `zr-menu'.")
  (define-key zr-menu [org-menu]
              (list 'menu-item "org-menu" zr-org-menu))
  :hook
  (org-mode . (lambda ()
                (modify-syntax-entry ?< "." org-mode-syntax-table)
                (modify-syntax-entry ?> "." org-mode-syntax-table)))
  :bind
  ( :map org-mode-map
    ("C-," . nil)
    ("C-'" . nil)
    ("C-c $" . org-cycle-agenda-files))
  ( :repeat-map zr-org-cycle-repeat-map
    ("$" . org-cycle-agenda-files))
  ( :repeat-map zr-org-block-repeat-map
    ("f" . org-next-block)
    ("b" . org-previous-block)
    ("c" . org-fold-hide-block-toggle)
    :exit
    ("u" . org-up-element))
  :custom
  (org-read-date-popup-calendar nil)
  (org-fontify-done-headline nil)
  ;; (org-replace-disputed-keys t "see `'org-disputed-keys'")
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-cycle-emulate-tab 'whitestart)
  (org-use-speed-commands t)
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  (org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (org-use-sub-superscripts '{})
  (org-startup-folded t)
  (org-cycle-hide-block-startup t)
  (org-refile-use-cache nil)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-archive-mark-done nil)
  (org-archive-location "_archive/%s::* Archive")
  (org-use-fast-todo-selection 'expert)
  (org-treat-S-cursor-todo-selection-as-state-change nil)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))
  (org-fast-tag-selection-include-todo t)
  (org-track-ordered-property-with-tag t)
  (org-tag-alist '(;; locale
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
  (org-log-done 'time)
  (org-log-into-drawer t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (cl-delete-if-not
    (lambda (pair)
      (locate-library (concat "ob-" (symbol-name (car pair)))))
    '((C . t)
      (R . nil)
      (awk . t)
      (calc . t)
      (ditaa . nil)
      (dot . nil)
      (emacs-lisp . t)
      (eshell . t)
      (gnuplot . t)
      (haskell . nil)
      (js . t)
      (latex . nil)
      (ledger . nil)
      (lua . t)
      (makefile . t)
      (ocaml . nil)
      (octave . nil)
      (org . t)
      (plantuml . t)
      (python . t)
      (ruby . nil)
      (screen . nil)
      (sed . t)
      (shell . t)
      (sql . t)
      (sqlite . t))))
  (dolist (mod '(org-tempo org-crypt org-protocol))
    (add-to-list 'org-modules mod)))

(use-package org-attach
  :custom
  (org-attach-archive-delete 'ask)
  :config
  (require 'org-attach-git))

(use-package org-clock
  :init
  (define-key zr-org-menu [org-clock-in]
              '(menu-item "clock-in" org-clock-in))
  (define-key zr-org-menu [org-clock-in-last]
              '(menu-item "clock-in-last" org-clock-in-last))
  (define-key zr-org-menu [org-clock-out]
              '(menu-item "clock-out" org-clock-out))
  (define-key zr-org-menu [org-clock-goto]
              '(menu-item "clock-goto" org-clock-goto))
  :custom
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist 'history)
  (org-clock-in-resume t)
  (org-timer-default-timer 5)
  (org-clock-clocked-in-display 'frame-title)
  :init
  (put 'org-timer-default-timer 'safe-local-variable #'natnump)
  :config
  (org-clock-persistence-insinuate))

(use-package org-crypt
  :config
  (org-crypt-use-before-save-magic)
  (add-to-list 'org-tag-alist (cons org-crypt-tag-matcher ?c) t)
  (add-to-list 'org-tags-exclude-from-inheritance org-crypt-tag-matcher))

(use-package org-list
  :custom
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-list-use-circular-motion t))

(use-package org-table
  ;; :hook (text-mode . turn-on-orgtbl)
  :custom
  ;; (org-table-header-line-p t)
  (org-table-use-standard-references t)
  (org-table-automatic-realign nil)
  ;; :config
  ;; https://emacs-china.org/t/org-9-6-5-org/24484
  ;; (advice-add #'org-string-width :before-until #'org--string-width-1)
  )

(use-package org-agenda
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :custom
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-start-on-weekday nil)
  (org-agenda-span 'day)
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up user-defined-up effort-up category-keep)
     (todo category-up effort-up)
     (tags category-up effort-up)
     (search category-up)))
  (org-agenda-window-setup 'current-window)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  ;; (org-agenda-use-tag-inheritance nil)
  ;; (org-agenda-ignore-properties '(effort appt stats category))
  (org-agenda-custom-commands
   '(("n" "Agenda and All Todos"
      ((agenda)
       (todo)))
     ("w" "Work" agenda ""
      ((org-agenda-files '("work.org"))))))
  :config
  (org-agenda-to-appt t))

(use-package org-capture
  :bind
  (:map global-map
        ("C-c c" . org-capture))
  :custom
  (org-capture-templates
   '(("c" "Default" entry (file "inbox.org")
      "* %?\n%U\n%i")
     ("a" "Agenda" entry (file "agenda.org")
      "* TODO %?\n%U\n%i")
     ("r" "Reference" entry (file "inbox.org")
      "* %?\n%U\n%i\n%a")
     ("l" "Clocking" entry (clock) nil)
     ;; Define a section
     ("w" "Work")
     ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
      "** TODO %?\n%U\n%i\n%a")
     ("wr" "Work report" entry (file+headline "work.org" "Reports")
      "** TODO %?\n%U\n%i\n%a"))))

(use-package org-src
  :custom
  (org-src-preserve-indentation t)
  :config
  (dolist (l '(("json" . json-ts)
               ("caddy" . caddyfile)
               ("lua" . lua-ts)
               ("py" . python-ts)
               ("yml" . yaml-ts)))
    (add-to-list 'org-src-lang-modes l)))

(use-package ol
  :commands org-insert-link-global
  :init
  (define-key zr-org-menu [org-store-link]
              '(menu-item "store-link" org-store-link))
  :bind
  ( :repeat-map zr-org-link-repeat-map
    ("n" . org-next-link)
    ("p" . org-previous-link)
    ("o" . org-open-at-point))
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive)
  (org-link-use-indirect-buffer-for-internals t)
  :config
  (setcdr (assoc 'file org-link-frame-setup) 'find-file))

(use-package ob
  :bind
  (:repeat-map zr-ob-repeat-map
               ("Z" . org-babel-switch-to-session)
               ("a" . org-babel-sha1-hash)
               ("c" . org-babel-check-src-block)
               ("d" . org-babel-demarcate-block)
               ("e" . org-babel-execute-maybe)
               ("f" . org-babel-tangle-file)
               ("g" . org-babel-goto-named-src-block)
               ("i" . org-babel-lob-ingest)
               ("j" . org-babel-insert-header-arg)
               ("k" . org-babel-remove-result-one-or-many)
               ("l" . org-babel-load-in-session)
               ("m" . org-babel-mark-block)
               ("n" . org-babel-next-src-block)
               ("o" . org-babel-open-src-block-result)
               ("p" . org-babel-previous-src-block)
               ("r" . org-babel-goto-named-result)
               ("t" . org-babel-tangle)
               ("u" . org-babel-goto-src-block-head)
               ("x" . org-babel-do-key-sequence-in-edit-buffer)
               ("z" . org-babel-switch-to-session-with-code)
               :exit
               ("E" . org-edit-special)
               ("I" . org-babel-view-src-block-info)
               ("N" . org-narrow-to-block)
               ("v" . org-babel-expand-src-block))
  :custom
  (org-plantuml-exec-mode 'plantuml)
  (org-babel-lua-mode 'lua-ts-mode)
  :config
  ;; (add-to-list 'display-buffer-alist
  ;;              `(,(regexp-quote org-babel-error-buffer-name)
  ;;                display-buffer-no-window
  ;;                (allow-no-window . t)))
  (put 'org-babel-tangle-use-relative-file-links 'safe-local-variable 'null)
  (setq org-babel-default-header-args
        (append '((:noweb . "yes")
                  (:comments . "link")
                  (:eval . "never-export")
                  (:results . "silent"))
                (cl-delete-if (lambda (a) (memq a '( :noweb :results
                                                :comments :eval)))
                              org-babel-default-header-args :key #'car)))
  (with-eval-after-load 'ob-org
    (dolist (l '("conf"
                 "conf-space"
                 "conf-unix"
                 "conf-windows"
                 "json"
                 "caddy"
                 "text"
                 "toml"
                 "yml"))
      (dolist (f '("org-babel-expand-body"
                   "org-babel-execute"))
        (defalias (intern (format "%s:%s" f l))
          (intern (concat f ":org"))))))
  (with-eval-after-load 'ob-plantuml
    (define-advice org-babel-plantuml-make-body (:filter-return (body) start-at-begin)
      "Move `@start' to first line, avoid define variable before start."
      (replace-regexp-in-string "\\([^z-a]+\\)\\(^@start.+\\)" "\\2\n\\1" body))
    (setq org-babel-default-header-args:plantuml
          (cons '(:results . "replace verbatim")
                (assq-delete-all :results org-babel-default-header-args:plantuml)))))

(use-package ox
  :custom
  (org-export-dispatch-use-expert-ui t)
  (org-export-coding-system 'utf-8)
  (org-export-with-author nil)
  (org-export-with-smart-quotes t)
  (org-export-with-sub-superscripts nil)
  (org-export-backends '(org html latex md ascii icalendar))
  (org-html-table-default-attributes '(:border "2" :cellspacing "0" :cellpadding "6" :rules "all" :frame "border"))
  (org-html-postamble nil)
  (org-latex-pdf-process '("tectonic %f"))
  (org-latex-default-class "ctexart")
  (org-latex-hyperref-template "\\hypersetup{
 pdfborder={0 0 0},
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c}, 
 pdflang={%L}}
")
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
  (package-quickstart t)
  (package-quickstart-file (locate-user-emacs-file "_package-quickstart.el"))
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

(use-package caddyfile-mode
  :if (package-installed-p 'caddyfile-mode)
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode))
  :hook
  (caddyfile-mode . (lambda ()
                      (kill-local-variable 'tab-width)
                      (kill-local-variable 'indent-tabs-mode))))

(use-package ahk-mode
  :if (package-installed-p 'ahk-mode)
  :mode "\\.ahk\\'"
  ;; [[elisp:(package-generate-autoloads "ahk-mode" (expand-file-name "ahk-mode" package-user-dir))]]
  :vc ( :url "https://github.com/zyt-zsn/ahk-mode"
        :lisp-dir "."
        :rev "9e293237eab1626f7c539dec8fd878a0308a96f2")
  :custom
  (ahk-chm-path (substitute-in-file-name "$USERPROFILE/scoop/apps/autohotkey/current/v2/AutoHotkey.chm"))
  (ahk-indent-offset 2))

(use-package html-ts-mode
  :init
  (put 'html-ts-mode-indent-offset 'safe-local-variable #'natnump))

(use-package lua-ts-mode
  :mode "\\.lua\\'"
  :custom
  (lua-ts-indent-offset 2))

(use-package nix-ts-mode
  :if (package-installed-p 'nix-ts-mode)
  :mode "\\.nix\\'"
  :config
  (setq-mode-local nix-ts-mode
                   buffer-file-coding-system 'prefer-utf-8-unix)
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("nix" . nix-ts))))

(use-package gdscript-mode
  :if (package-installed-p 'gdscript-mode)
  :custom
  (gdscript-debug-port 6006)
  (gdscript-use-tab-indents t)
  (gdscript-indent-offset 4)
  (gdscript-godot-executable "godot-mono")
  (gdscript-gdformat-save-and-format t)
  :config
  (when (treesit-language-available-p 'gdscript)
    (with-eval-after-load 'eglot
      (setq eglot-server-programs
            (cons '((gdscript-mode gdscript-ts-mode) "127.0.0.1" 6005)
                  (assq-delete-all 'gdscript-mode eglot-server-programs))))
    (add-to-list 'major-mode-remap-alist
                 '(gdscript-mode . gdscript-ts-mode))))

(use-package magit
  :if (package-installed-p 'magit)
  :custom
  (magit-commit-show-diff nil)
  (magit-branch-direct-configure nil)
  (magit-refresh-status-buffer nil)
  (magit-tramp-pipe-stty-settings 'pty)
  (magit-wip-mode-lighter nil))

(use-package with-editor
  :if (and (not (eq system-type 'windows-nt))
           (package-installed-p 'with-editor))
  :custom
  (shell-command-with-editor-mode t)
  :hook
  ((#1=( shell-mode
         eshell-mode
         term-exec-hook)
       . with-editor-export-editor)
   (#1# . with-editor-export-git-editor)))

(use-package dape
  :if (package-installed-p 'dape)
  :custom
  (dape-buffer-window-arrangement 'gud)
  :config
  (add-hook 'dape-compile-hook 'kill-buffer))

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

(use-package anki-editor
  :if (package-installed-p 'anki-editor)
  :vc (:url "https://github.com/anki-editor/anki-editor" :rev :newest))

(use-package plantuml-mode
  :if (package-installed-p 'plantuml-mode)
  :custom
  (plantuml-default-exec-mode 'executable)
  (plantuml-indent-level 2))

(use-package gnuplot
  :if (package-installed-p 'gnuplot)
  :mode ("\\.gp\\'" . gnuplot-mode))

;; https://karthinks.com/software/avy-can-do-anything/
(use-package avy
  :if (package-installed-p 'avy)
  :init
  (define-key zr-menu [avy-resume]
              '(menu-item "avy-resume" avy-resume))
  :bind
  (("M-g M-g" . avy-goto-line)
   ("C-," . avy-goto-char-timer))
  ( :map isearch-mode-map
    ("M-j" . avy-isearch))
  :config
  (defun zr-avy-action-comment (pt)
    (if (> pt (point))
        (comment-or-uncomment-region (point) pt)
      (comment-or-uncomment-region pt (point))))
  (setf (alist-get ?\; avy-dispatch-alist) 'zr-avy-action-comment)
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package marginalia :after embark :defer 0
  :if (package-installed-p 'marginalia)
  :bind
  ( :map minibuffer-local-map
    ("M-A" . marginalia-cycle))
  :config
  (marginalia-mode))

;; https://karthinks.com/software/fifteen-ways-to-use-embark/
(use-package embark :after minibuffer :defer 1
  :if (package-installed-p 'embark)
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  ( :map icomplete-fido-mode-map
    ("C-." . nil)
    :map icomplete-minibuffer-map
    ("C-." . nil))
  :custom
  (embark-quit-after-action nil)
  :config
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; impact performance
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (when-let* ((mixed (memq 'embark-mixed-indicator embark-indicators)))
    (setcar mixed 'embark-minimal-indicator))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package denote
  :if (package-installed-p 'denote)
  :init
  (put 'denote-directory 'safe-local-variable 'stringp)
  (with-eval-after-load 'dired
    (bind-keys
     :map zr-dired-spc-prefix-map
     ("r" . denote-dired-rename-files)
     ("R" . denote-dired-rename-marked-files-with-keywords)))
  (with-eval-after-load 'org-capture
    (dolist (c '(("s" "denote subdir" plain
                  (file denote-last-path)
                  (function
                   (lambda ()
                     (denote-org-capture-with-prompts :title :keywords :subdirectory)))
                  . #1=( :no-save t
                         :immediate-finish nil
                         :kill-buffer t
                         :jump-to-captured t))
                 ("d" "denote date" plain
                  (file denote-last-path)
                  (function
                   (lambda ()
                     (denote-org-capture-with-prompts :title :keywords :date)))
                  . #1#)
                 ("n" "denote" plain
                  (file denote-last-path)
                  #'denote-org-capture
                  . #1#)
                 ("j" "denote journal" entry
                  (function denote-journal-extras-new-or-existing-entry)
                  "* %<%T>\n%a\n%i\n%?"
                  . #1#)))
      (add-to-list 'org-capture-templates c))
    (setq denote-org-capture-specifiers "%l\n%i\n%?"))
  :custom
  (denote-backlinks-show-context t)
  (denote-known-keywords '("emacs" "entertainment" "reading" "studying" "work"))
  (denote-date-prompt-use-org-read-date t)
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompts '(title keywords))
  (denote-date-format nil)
  (denote-excluded-directories-regexp "_archive")
  :hook
  ((dired-mode . denote-dired-mode)
   (find-file . denote-fontify-links-mode-maybe))
  :config
  (setq denote-directory org-directory)
  (define-advice denote-rename-file-and-buffer (:before-while (old new) rename-dir)
    "Rename directory in dired."
    (if (and (file-directory-p old)
             (derived-mode-p 'dired-mode))
        (dired-rename-file old new nil)
      t))
  (unless (file-exists-p denote-directory)
    (make-directory denote-directory))
  (defun zr-denote-region-org-structure-template (_beg _end)
    (when (derived-mode-p 'org-mode)
      (activate-mark)
      (call-interactively 'org-insert-structure-template)))
  (add-hook 'denote-region-after-new-note-functions #'zr-denote-region-org-structure-template)
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

(use-package separedit
  :if (package-installed-p 'separedit)
  :hook (separedit-buffer-creation . auto-fill-mode)
  :bind ("C-c '" . separedit)
  :custom
  (separedit-default-mode 'org-mode)
  (separedit-preserve-string-indentation t)
  (separedit-continue-fill-column t)
  (separedit-remove-trailing-spaces-in-comment t)
  :config
  (with-eval-after-load 'viper
    (add-hook 'separedit-buffer-creation-hook
              #'viper-change-state-to-insert)))

(use-package lentic
  :if (package-installed-p 'lentic))

(use-package powershell
  :if (package-installed-p 'powershell)
  :bind
  ( :map powershell-mode-map
    ("M-`" . nil)
    ("C-`" . powershell-escape-selection))
  :config
  ;; powershell 5.1
  (setq-mode-local powershell-mode
                   buffer-file-coding-system 'utf-8-with-signature))

(use-package sqlformat
  :if (package-installed-p 'sqlformat)
  :init
  (put 'sqlformat-args 'safe-local-variable 'listp)
  :bind
  (:map sql-mode-map
        ("C-c C-f" . sqlformat))
  :custom
  (sqlformat-command 'sqlfluff))

(use-package flymake-sqlfluff
  :if (and (package-installed-p 'flymake-sqlfluff)
           (executable-find "sqlfluff"))
  :hook (sql-mode . flymake-sqlfluff-load))

(use-package envrc
  :if (package-installed-p 'envrc)
  :bind
  (:map envrc-mode-map
        ("C-c e" . envrc-command-map)))

(use-package buffer-env
  :if (package-installed-p 'buffer-env)
  :hook
  (((hack-local-variables
     comint-mode)
    . buffer-env-update)))

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

(use-package verb
  :if (package-installed-p 'verb)
  :bind
  ( :map org-mode-map
    ("C-c C-/" . verb-command-map)))

(use-package emms
  :if (package-installed-p 'emms)
  :custom
  (emms-directory (locate-user-emacs-file "_emms"))
  (emms-info-functions '(emms-info-native))
  (emms-info-asynchronously t)
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (emms-browser-thumbnail-small-size 64)
  (emms-browser-thumbnail-medium-size 128)
  (emms-source-playlist-default-format 'm3u)
  :config
  (emms-minimalistic)
  (require 'emms-history)
  (emms-history-load)
  (unless zr-sys-android-p
    (setq emms-player-list '(emms-player-mpv)))
  (add-to-list 'emms-track-initialize-functions #'emms-info-initialize-track)
  (setq emms-track-description-function #'emms-info-track-description)
  (require 'emms-cache)
  (emms-cache 1)
  (when (locate-library "init-emms")
    (require 'init-emms)))

(use-package mpvi :after emms
  :if (package-installed-p 'mpvi))

(use-package devdocs-browser
  :if (package-installed-p 'devdocs-browser)
  :bind
  (:map help-map
        ("u" . devdocs-browser-open)
        ("U" . devdocs-browser-open-in))
  :config
  (add-to-list 'browse-url-handlers
               `(,(concat "\\`file://"
                          (when (memq system-type '(windows-nt ms-dos))
                            "/")
                          (regexp-quote devdocs-browser-data-directory))
                 . eww-browse-url)))

(use-package devdocs
  :if (package-installed-p 'devdocs)
  :bind
  (:map help-map
        ("u" . 'devdocs-lookup))
  ( :map devdocs-mode-map
    ("TAB" . shr-next-link)
    ("M-TAB" . shr-previous-link))
  :init
  (put 'devdocs-current-docs 'safe-local-variable 'listp)
  (cl-loop
   for (langs . docs)
   in '(((c-mode c-ts-mode) "c")
        ((c++-mode c++-ts-mode) "cpp")
        ((python-base-mode) "python")
        ((html-mode html-ts-mode css-base-mode js-base-mode)
         "html" "css" "javascript"))
   do (dolist (lang langs)
        (mode-local-bind (list (cons 'devdocs-current-docs docs))
                         '(mode-variable-flag t)
                         lang))))

(use-package xeft
  :if (package-installed-p 'xeft))

(use-package khoj
  :if (package-installed-p 'khoj))

(use-package dired-duplicates
  :if (package-installed-p 'dired-duplicates))

(use-package disk-usage
  :if (package-installed-p 'disk-usage)
  :config
  (when (executable-find "du")
    (setq disk-usage-du-command "du")
    (custom-reevaluate-setting 'disk-usage-directory-size-function)))

(use-package aria2
  :if (package-installed-p 'aria2)
  :init
  (define-key zr-menu [aria2-download-list]
              '(menu-item "aria2-downloads-list" aria2-downloads-list))
  :config
  (let ((auth (car (auth-source-search :host "aria2.localhost"))))
    (setq aria2-rcp-secret (auth-info-password auth)
          aria2-rcp-listen-port (string-to-number (plist-get auth :port)))))

(use-package org-srs
  :if (package-installed-p 'org-srs)
  :hook (org-mode . org-srs-embed-overlay-mode)
  :bind
  ( :map org-mode-map
    ("<f5>" . org-srs-review-rate-easy)
    ("<f6>" . org-srs-review-rate-good)
    ("<f7>" . org-srs-review-rate-hard)
    ("<f8>" . org-srs-review-rate-again))
  :config
  (when zr-sys-android-p
    (setq org-srs-item-confirm #'org-srs-item-confirm-command)
    (org-srs-ui-mode +1)))

(use-package ox-pandoc :defer 2
  :if (package-installed-p 'ox-pandoc)
  :after org
  :custom
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "tectonic"))))

;; [[elisp:(custom-reevaluate-setting 'org-pandoc-options)]]

(use-package nov
  ;; https://emacs-china.org/t/emacs-epub/4713/12
  :if (package-installed-p 'nov)
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-unzip-program archive-7z-program)
  (nov-variable-pitch nil)
  (nov-unzip-args '("x" filename))
  :bind
  ( :map nov-mode-map
    ("<left>" . nov-scroll-down)
    ("<right>" . nov-scroll-up)
    ("k" . kill-current-buffer)
    ("&" . zr-shell-do-open))
  :config
  (defun zr-nov-toggle-header-line ()
    (interactive)
    (let ((v 'nov-header-line-format))
      (if (local-variable-p v)
          (kill-local-variable v)
        (setq-local nov-header-line-format nil))
      (nov-render-document)))
  (defun zr-nov-goto-toc ()
    "Go to the TOC index and render the TOC document."
    (interactive)
    (let ((id nov-documents-index)
          (cnt 1)
          (match 1))
      (nov-goto-toc)
      (while (and match (< cnt id))
        (setq match (text-property-search-forward 'shr-tab-stop nil nil t)
              cnt (1+ cnt)))
      (goto-char (prop-match-beginning match))))
  (bind-keys
   :map nov-mode-map
   ("T" . zr-nov-goto-toc)
   ("i" . zr-nov-toggle-header-line))
  (with-eval-after-load 'tab-line
    (push 'nov-mode tab-line-exclude-modes))
  ;; advice for 7z cli cannot split "-o" and `direcotry'
  (define-advice nov-unzip-epub (:around (orig-fun &rest args) extract-with-7z)
    (let ((nov-unzip-args
           (append nov-unzip-args
                   (list (format "-o%s" (car args))))))
      (apply orig-fun args))))

(use-package fdroid
  :if (package-installed-p 'fdroid)
  :vc (:url "https://github.com/migalmoreno/fdroid.el"))

(use-package agent-shell
  :if (package-installed-p 'agent-shell)
  :config
  (let ((codex-host "www.88code.ai"))
    (setq agent-shell-openai-codex-environment
          (agent-shell-make-environment-variables
           "OPENAI_BASE_URL" (format "https://%s/openai/v1" codex-host))
          agent-shell-openai-authentication
          (agent-shell-openai-make-authentication
           :codex-api-key (lambda () (auth-source-pass-get "emacs_api" (format "websites/%s/linuxdo" codex-host)))))))

(use-package claude-code-ide
  :if (package-installed-p 'claude-code-ide)
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :custom
  (claude-code-ide-terminal-backend 'eat))

(use-package gptel
  :if (package-installed-p 'gptel)
  :init
  (define-key zr-menu [gptel] '(menu-item "gptel" gptel-send))
  :custom
  (gptel-curl-extra-args '("--compressed" "-H" "x-litellm-tags: general"))
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context t)
  :config
  ;; remove chatgpt. ref: https://github.com/karthink/gptel/issues/649#issuecomment-3067343094
  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))
  (let ((prompt "@user\n")
        (response "@assistant\n"))
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist) prompt)
    (setf (alist-get 'org-mode gptel-response-prefix-alist) response)
    (add-hook 'gptel-mode-hook
              (lambda ()
                (gptel-highlight-mode 1)
                (when (eq major-mode 'org-mode)
                  (if gptel-mode
                      (setq
                       imenu-create-index-function
                       #'imenu-default-create-index-function
                       imenu-generic-expression
                       `(("p" ,(rx bol (literal prompt) (group (1+ any))) 1)
                         ("r" ,(rx bol (literal response) (group (1+ any))) 1)))
                    (setq imenu-create-index-function #'org-imenu-get-tree))))))
  (require 'ob-ref)
  (when-let* (((bound-and-true-p zr-dotfiles-dir))
              (pls-path (expand-file-name ".global.pls" zr-dotfiles-dir))
              ((file-exists-p pls-path))
              (zr-local-pls (plstore-open pls-path))
              (host (plist-get (cdr (plstore-get zr-local-pls "host")) :main))
              (litellm-path (expand-file-name "litellm/20251210T201813--litellm__server.org" zr-dotfiles-dir))
              (main-models (if (file-exists-p litellm-path)
                               (mapcar #'intern
                                       (cddr (org-babel-ref-resolve
                                              (format "%s:models-tbl[,0]"
                                                      litellm-path))))
                             '( glm-4.6
                                qwen3-coder-plus
                                qwen3-235b-a22b-instruct
                                qwen3-235b-a22b-thinking-2507
                                qwen3-vl-plus
                                qwen3-max
                                kimi-k2
                                kimi-k2-0905
                                gemini-2.5-pro
                                gemini-3-pro
                                gpt-5
                                grok-4.1
                                deepseek-r1
                                deepseek-v3.2))))
    (plstore-close zr-local-pls)
    (gptel-make-gemini "gemini"
      :host (concat "cpa." host)
      :key #'gptel-api-key
      :stream t)
    (setq gptel-model (car main-models)
          gptel-backend (gptel-make-openai "general"
                          :host (concat "litellm." host)
                          :key #'gptel-api-key
                          :endpoint "/v1/chat/completions"
                          :models main-models
                          :stream t))))

(use-package keyfreq
  :if (package-installed-p 'keyfreq)
  :hook
  (emacs-startup
   (emacs-startup . keyfreq-autosave-mode))
  :custom
  ;; ref: https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-keyfreq.el
  (keyfreq-excluded-regexp
   '("^abort-"
     "^cua-"
     "^describe-"
     "^dired"
     "^gnus-"
     "^icomplete-"
     "^ido-"
     "^isearch-"
     "^keyboard-"
     "^keyfreq-"
     "^org-"
     "^save-"
     "scroll-\\(up\\|down\\)"
     "^viper-"
     "^y-or-n-"
     "backward-"
     "emms-"
     "forward-"
     "next"
     "prev"
     "self-insert"))
  :config
  (setq keyfreq-excluded-commands
        '(clipboard-kill-ring-save
          comint-magic-space
          comint-send-input
          electric-newline-and-maybe-indent
          electric-pair-delete-pair
          eval-buffer
          exit-minibuffer
          ffip
          goto-line
          hippie-expand
          ignore
          indent-new-comment-line
          ispell-minor-check
          js-mode
          kill-sentence
          left-char
          minibuffer-complete
          minibuffer-complete-and-exit
          minibuffer-keyboard-quit
          move-beginning-of-line
          move-end-of-line
          mwheel-scroll
          newline-and-indent
          other-window
          package-menu-execute
          pcomplete
          push-button
          pwd
          quit-window
          recenter-top-bottom
          right-char
          suspend-frame
          term-send-raw
          undefined ;; lambda function
          undo
          undo-redo
          yank)))

(use-package diminish
  :if (package-installed-p 'diminish)
  :init
  (with-eval-after-load 'abbrev
    (diminish 'abbrev-mode " A"))
  (with-eval-after-load 'completion-preview
    (diminish 'completion-preview-mode))
  (with-eval-after-load 'hideshow
    (diminish 'hs-minor-mode))
  (with-eval-after-load 'subword
    (diminish 'subword-mode))
  :config
  (diminish 'visual-line-mode))

;; https://www.ifarchive.org/indexes/if-archive/games/zcode/
;; https://ifdb.org/
(use-package malyon)

(use-package el-search :defer 1
  :if (package-installed-p 'el-search)
  :bind
  ( :repeat-map zr-el-search-repeat-map
    ("s" . el-search-pattern)
    ("r" . el-search-pattern-backward))
  :config
  (el-search-install-bindings-under-prefix [(meta ?s) ?e]))

(use-package atomic-chrome
  :if (package-installed-p 'atomic-chrome)
  :vc (:url "https://github.com/KarimAziev/atomic-chrome" :rev :newest)
  :hook
  (atomic-chrome-edit-done #'iconify-frame)
  :custom
  (atomic-chrome-buffer-open-style 'full)
  (atomic-chrome-max-filename-size 120)
  (atomic-chrome-url-major-mode-alist
   '(("\\`https://www\\.freecodecamp\\.org/learn/javascript" . js-ts-mode)))
  :config
  (add-hook 'atomic-chrome-edit-mode-hook
            (lambda () (when viper-mode (viper-mode))) 1))

;;; init.el ends here

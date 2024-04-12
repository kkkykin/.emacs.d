;;; init.el --- Personal Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-to-list 'load-path (file-name-concat user-emacs-directory "lisp"))
(require 'init-misc)

(use-package emacs
  :hook
  ((text-mode . visual-line-mode)
   (next-error . recenter)
   (window-setup . my/setup-faces))
  :bind-keymap ("C-x j" . my/global-prefix-map)
  :bind
  ([remap eval-expression] . pp-eval-expression)
  ([remap eval-last-sexp] . pp-eval-last-sexp)
  ([remap upcase-word] . upcase-dwim)
  ([remap downcase-word] . downcase-dwim)
  ([remap capitalize-word] . capitalize-dwim)
  (:map my/global-prefix-map
        ("s" . scratch-buffer))
  :custom
  (inhibit-splash-screen t)
  (indicate-buffer-boundaries 'left)
  (initial-major-mode 'fundamental-mode)
  (disabled-command-function nil)
  (system-time-locale "C")
  (use-dialog-box nil)
  (use-package-always-defer t)
  (truncate-lines t)
  (mark-ring-max 6)
  (global-mark-ring-max 8)
  (set-mark-command-repeat-pop t)
  (scroll-preserve-screen-position t)
  (scroll-margin 0)
  (scroll-conservatively 97)
  (make-cursor-line-fully-visible nil)
  (blink-cursor-mode nil)
  (scroll-bar-mode nil)
  (column-number-mode t)
  (shift-select-mode nil)
  (global-prettify-symbols-mode t)
  (prettify-symbols-unprettify-at-point t)
  (display-line-numbers-type 'relative)
  (use-short-answers t)
  (word-wrap-by-category t)
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (what-cursor-show-names t)
  (redisplay-skip-fontification-on-input t)
  (kill-read-only-ok t)
  (kill-do-not-save-duplicates t)
  (duplicate-region-final-position 1)
  (indent-tabs-mode nil)
  (tab-width 4)
  (switch-to-buffer-obey-display-actions t)
  (shell-command-dont-erase-buffer 'end-last-out)
  (async-shell-command-buffer 'rename-buffer)
  (async-shell-command-display-buffer nil)
  (shell-command-default-error-buffer "*Shell Command Error*")
  ;; (mouse-1-click-follows-link -450 "click set point, long press do action")

  ;; long line performance https://emacs-china.org/t/topic/25811/9
  (bidi-display-reordering nil)
  (bidi-inhibit-bpa t)
  (long-line-threshold 1000)
  (large-hscroll-threshold 1000)
  (syntax-wholeline-max 1000)
  :config
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (pcase system-type
    ('windows-nt (require 'init-winnt))
    ('gnu/linux (require 'init-linux))
    ('android (require 'init-android))))

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
  :hook (emacs-startup . fido-mode)
  :config
  (advice-add 'icomplete--fido-mode-setup :after-while
              (lambda (&rest r)
                (setq-local completion-styles
                            '(initials partial-completion flex)))))

(use-package completion-preview
  :if (package-installed-p 'completion-preview)
  :hook (prog-mode eshell-mode inferior-emacs-lisp-mode))

(use-package viper
  :init (setq viper-inhibit-startup-message t
              viper-expert-level 5
              viper-vi-style-in-minibuffer nil
              viper-vi-state-cursor-color nil
              viper-buffer-search-char ?g
              viper-case-fold-search t
              viper-shift-width 2
              viper-auto-indent t
              viper-electric-mode t
              viper-ex-style-motion nil
              viper-ex-style-editing nil
              viper-ESC-moves-cursor-back nil
              viper-mode t)
  :hook (window-setup
         (edebug-mode . viper-change-state-to-emacs))
  :bind
  (:map viper-insert-global-user-map
        ("M-p" . viper-insert-prev-from-insertion-ring)
        ("M-n" . viper-insert-next-from-insertion-ring)
        ("C-t" . transpose-chars)
        ("C-d" . delete-char)
        ("C-w" . kill-region)
        ("C-v" . scroll-up))
  (:map viper-vi-global-user-map
        ("zE" . duplicate-dwim)
        ("zf" . org-open-at-point-global)
        ("zr" . re-builder)
        ("zR" . rename-visited-file)
        ("zt" . transpose-sentences)
        ("zT" . transpose-paragraphs)
        ("C-y" . yank)
        ("C-f" . forward-char)
        ("C-b" . backward-char)
        ("C-e" . move-end-of-line)
        ("C-u" . universal-argument)
        ("M-p" . viper-prev-destructive-command)
        ("M-n" . viper-next-destructive-command)
        ("C-v" . scroll-up))
  :custom
  (viper-want-ctl-h-help t)
  (viper-no-multiple-ESC nil)
  (ex-cycle-other-window nil)
  (viper-syntax-preference 'emacs)
  :custom-face
  (viper-minibuffer-emacs ((t (:background "unspecified" :foreground "unspecified"))))
  :config
  (keymap-unset viper-insert-basic-map "C-c M-n")
  (keymap-unset viper-insert-basic-map "C-c M-p")
  (when my/sys-winnt-p
    (add-hook 'viper-vi-state-hook (lambda () (w32-set-ime-open-status nil))))
  (fset 'viper-del-backward-char-in-insert 'backward-delete-char-untabify)
  (with-eval-after-load 'elec-pair
    (keymap-set viper-insert-global-user-map "<backspace>"
                (alist-get 127 (cdr electric-pair-mode-map))))
  (put 'viper-setup-master-buffer 'safe-local-eval-function t)
  (put 'viper-mode-string 'risky-local-variable t)
  (add-face-text-property 0 (length viper-emacs-state-id) '(:inverse-video t) nil viper-emacs-state-id)
  (setq global-mode-string (delq 'viper-mode-string global-mode-string))
  (unless (memq 'viper-mode-string mode-line-format)
    (setcdr (cddr mode-line-format) (cons 'viper-mode-string (cdddr mode-line-format))))
  (setopt
   viper-major-mode-modifier-list
   (append '((sql-interactive-mode insert-state viper-comint-mode-modifier-map)
             (sql-interactive-mode vi-state viper-comint-mode-modifier-map))
           viper-major-mode-modifier-list))
  (dolist (mode '( change-log-mode diff-mode dun-mode org-mode
                   outline-mode vc-git-log-edit-mode reb-mode
                   sql-interactive-mode))
    (setq viper-vi-state-mode-list (delq mode viper-vi-state-mode-list))
    (add-to-list 'viper-insert-state-mode-list mode)))

(use-package help
  :custom
  (help-window-select t "Switch to help buffers automatically")
  (help-window-keep-selected t)
  (help-enable-symbol-autoload t)
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

(use-package info
  :custom
  (Info-directory-list (cons (expand-file-name "info" user-emacs-directory)
                             Info-directory-list)))

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :config
  (when (file-exists-p my/bookmark-shared-file)
    (bookmark-load my/bookmark-shared-file nil t)
    (advice-add 'bookmark-save :around 'my/advice-bookmark-save)))

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
  :unless my/sys-android-p
  :hook prog-mode
  :custom
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
  ;; :hook (org-mode prog-mode comint-mode)
  :hook (emacs-startup . global-hl-line-mode)
  :custom
  (global-hl-line-sticky-flag t))

(use-package display-line-numbers
  :unless my/sys-android-p
  :hook (prog-mode conf-mode edmacro-mode))

(use-package subword
  :unless my/sys-android-p
  :hook prog-mode)

(use-package glasses
  :unless my/sys-android-p
  ;; :hook prog-mode
  :custom
  (glasses-uncapitalize-p t)
  (glasses-separate-parentheses-p nil))

(use-package re-builder
  :custom
  (reb-re-syntax 'string)
  :config
  (defvar-keymap my/re-builder-repeat-mode
    :repeat t
    "s" #'reb-next-match
    "r" #'reb-prev-match))

(use-package isearch
  :custom
  (isearch-lazy-count t)
  (isearch-allow-scroll t)
  (isearch-yank-on-move 'shift)
  (isearch-repeat-on-direction-change t)
  :config
  (require 'transient)
  (transient-define-prefix my/isearch-menu ()
    "isearch Menu. http://yummymelon.com/devnull/improving-emacs-isearch-usability-with-transient.html"
    [["Edit Search String"
      ("e"
       "Edit the search string (recursive)"
       isearch-edit-string
       :transient nil)
      ("w"
       "Pull next word or character word from buffer"
       isearch-yank-word-or-char
       :transient nil)
      ("s"
       "Pull next symbol or character from buffer"
       isearch-yank-symbol-or-char
       :transient nil)
      ("l"
       "Pull rest of line from buffer"
       isearch-yank-line
       :transient nil)
      ("y"
       "Pull string from kill ring"
       isearch-yank-kill
       :transient nil)
      ("t"
       "Pull thing from buffer"
       isearch-forward-thing-at-point
       :transient nil)]

     ["Replace"
      ("q"
       "Start ‘query-replace’"
       isearch-query-replace
       :if-nil buffer-read-only
       :transient nil)
      ("x"
       "Start ‘query-replace-regexp’"
       isearch-query-replace-regexp
       :if-nil buffer-read-only     
       :transient nil)]]

    [["Toggle"
      ("X"
       "Toggle regexp searching"
       isearch-toggle-regexp
       :transient nil)
      ("S"
       "Toggle symbol searching"
       isearch-toggle-symbol
       :transient nil)
      ("W"
       "Toggle word searching"
       isearch-toggle-word
       :transient nil)
      ("F"
       "Toggle case fold"
       isearch-toggle-case-fold
       :transient nil)
      ("L"
       "Toggle lax whitespace"
       isearch-toggle-lax-whitespace
       :transient nil)]

     ["Misc"
      ("o"
       "occur"
       isearch-occur
       :transient nil)]])
  (keymap-set isearch-mode-map "C-h t" 'my/isearch-menu))

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
  (add-to-list 'auto-mode-alist
               '("[^/]\\.dired\\'" . dired-virtual-mode)))

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
        ("b" . 'speedbar))
  :config
  (setopt speedbar-supported-extension-expressions
          (append '(".sql")
                  speedbar-supported-extension-expressions))
  (when (require 'init-misc)
    (keymap-set speedbar-file-key-map "=" #'my/speedbar-item-diff)
    (keymap-set speedbar-file-key-map "(" #'my/speedbar-show-unknown-files)))

(use-package electric
  :custom
  (electric-layout-rules '())
  :hook ((text-mode fundamental-mode) . (lambda () (electric-pair-local-mode -1)))
  :config
  (electric-pair-mode)
  (electric-layout-mode))

(use-package windmove
  :unless my/sys-android-p
  :hook emacs-startup
  :custom
  (windmove-wrap-around t))

(use-package server :defer 5
  :custom
  (server-client-instructions nil)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package autorevert
  :custom
  (auto-revert-remote-files t))

(use-package diff
  :custom
  (diff-add-log-use-relative-names t))

(use-package grep
  :config
  (when (string= find-program "fd")
    (advice-add 'rgrep-default-command :around
                #'my/advice-rgrep-default-command-maybe-fd)))

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

(use-package find-dired
  :config
  (when (string= find-program "fd")
    (advice-add 'find-dired-with-command :filter-args #'my/advice-find-dired-with-command-maybe-fd)
    (setq find-name-arg "-g"
          find-ls-option '("-X busybox ls -ldh {} ;" . "-ldh"))))

(use-package etags-regen
  :if (and (package-installed-p 'etags-regen)
           (not my/sys-android-p))
  :hook emacs-startup)

(use-package abbrev
  :custom
  (abbrev-suggest t))

(use-package skeleton
  :custom
  (skeleton-further-elements '((abbrev-mode nil)))
  (skeleton-pair t))

;; [[https://github.com/yilkalargaw/emacs-native-snippets]]
(use-package tempo
  :autoload tempo-define-template
  :bind
  ("M-g M-n" . 'tempo-forward-mark)
  ("M-g M-p" . 'tempo-backward-mark)
  :config
  (defvar-keymap my/tempo-repeat-map
    :repeat t
    "n" #'tempo-forward-mark
    "p" #'tempo-backward-mark))

;; [[info:autotype]]
(use-package auto-insert
  :unless my/sys-android-p
  :hook emacs-startup
  :custom
  (auto-insert-directory (file-name-concat user-emacs-directory "insert/")))

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
               ("<left>" . windmove-left)
               ("<right>" . windmove-right)
               ("<up>" . windmove-up)
               ("<down>" . windmove-down)
               ("C-<left>" . windmove-delete-left)
               ("C-<right>" . windmove-delete-right)
               ("C-<up>" . windmove-delete-up)
               ("C-<down>" . windmove-delete-down)
               ("C-h" . windmove-swap-states-left)
               ("C-l" . windmove-swap-states-right)
               ("C-k" . windmove-swap-states-up)
               ("C-j" . windmove-swap-states-down)
               ("h" . windmove-display-left)
               ("l" . windmove-display-right)
               ("k" . windmove-display-up)
               ("j" . windmove-display-down)
               ("0" . windmove-display-same-window)
               ("f" . windmove-display-new-frame)
               ("t" . windmove-display-new-tab)
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
  (defvar-keymap my/structure-repeat-map
    :repeat (:enter ( treesit-beginning-of-defun beginning-of-defun
                      treesit-end-of-defun end-of-defun
                      indent-pp-sexp prog-indent-sexp
                      python-nav-backward-up-list backward-up-list
                      python-shell-send-defun eval-defun))
    "r" #'raise-sexp
    "i" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (pcase major-mode
            ('emacs-lisp-mode (indent-pp-sexp))
            (_ (prog-indent-sexp))))
    "k" #'kill-sexp
    "<backspace>" #'backward-kill-sexp
    "SPC" #'mark-sexp
    "t" #'transpose-sexps
    "s" #'delete-pair
    "(" #'insert-parentheses
    "'" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (insert-pair nil ?\' ?\'))
    "\"" (lambda ()
           (interactive)
           (setq repeat-map 'my/structure-repeat-map)
           (insert-pair nil ?\" ?\"))
    "<" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (insert-pair nil ?\< ?\>))
    "[" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (insert-pair nil ?\[ ?\]))
    "{" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (insert-pair nil ?\{ ?\}))
    "/" #'undo
    "w" #'hs-show-all
    "z" #'hs-hide-all
    "c" #'hs-toggle-hiding
    "u" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (pcase major-mode
            ((guard (memq major-mode '(python-ts-mode)))
             (python-nav-backward-up-list))
            (_ (backward-up-list))))
    "d" #'down-list
    "n" #'forward-list
    "p" #'backward-list
    "f" #'forward-sexp
    "b" #'backward-sexp
    "a" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (pcase major-mode
            ((guard (memq major-mode '(python-ts-mode)))
             (treesit-beginning-of-defun))
            (_ (beginning-of-defun))))
    "e" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (pcase major-mode
            ((guard (memq major-mode '(python-ts-mode)))
             (treesit-end-of-defun))
            (_ (end-of-defun))))
    "x" (lambda ()
          (interactive)
          (setq repeat-map 'my/structure-repeat-map)
          (pcase major-mode
            ((guard (memq major-mode '(python-ts-mode)))
             (python-shell-send-defun))
            (_ (eval-defun))))))

(use-package elide-head
  :unless my/sys-android-p
  :hook prog-mode)

;; https://karthinks.com/software/simple-folding-with-hideshow/
(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)
         ((ediff-prepare-buffer vc-before-checkin) . turn-off-hideshow)))

(use-package paren
  :custom
  (show-paren-predicate '(or (not (derived-mode . special-mode))
                             (major-mode . Info-mode)))
  (show-paren-when-point-in-periphery t)
  (show-paren-context-when-offscreen 'overlay))

(use-package which-func :defer 5
  :unless my/sys-android-p
  :config
  (when my/sys-android-p
    (setq which-func-display 'header))
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
  (project-vc-include-untracked nil)
  (project-kill-buffers-display-buffer-list t))

(use-package vc
  :custom
  (vc-display-status 'no-backend)
  (vc-handled-backends '(Git SVN))
  (vc-command-messages 'log))

(use-package add-log
  :bind
  (:map change-log-mode-map
        ("(" . skeleton-pair-insert-maybe))
  :custom
  (add-log-keep-changes-together t)
  (change-log-version-info-enabled t))

(use-package elisp-mode
  :config
  (when (require 'init-prog)
    (add-hook 'emacs-lisp-mode-hook 'my/custom-imenu-exp)))

(use-package python
  :custom
  (python-indent-block-paren-deeper t)
  (python-shell-interpreter-args "-i -X utf-8")
  (python-shell-dedicated t))

(use-package flymake
  :hook sh-mode
  :bind
  (:map flymake-mode-map
        ("C-x `" . flymake-goto-next-error))
  (:repeat-map my/flymake-repeat-map
               ("n" . flymake-goto-next-error)
               ("p" . flymake-goto-prev-error))
  :custom
  (flymake-show-diagnostics-at-end-of-line t))

(use-package gud
  :custom
  (gud-highlight-current-line t))

(use-package bs
  :bind
  ("C-x C-b" . bs-show)
  ("C-x C-<left>" . bs-cycle-previous)
  ("C-x C-<right>" . bs-cycle-next)
  (:repeat-map my/bs-repeat-map
               ("<left>" . bs-cycle-previous)
               ("<right>" . bs-cycle-next))
  :config
  (keymap-set bs-mode-map "i"
              (lambda () (interactive)
                (bs-kill)
                (ibuffer)
                (ibuffer-switch-to-saved-filter-groups "default")))
  (dolist (conf '(("dired" nil nil nil
                   (lambda (buf)
                     (with-current-buffer buf
                       (not (eq major-mode 'dired-mode))))
                   nil)
                  ("SQL" nil nil nil
                   (lambda (buf)
                     (with-current-buffer buf
                       (not (memq major-mode
                                  '(sql-interactive-mode sql-mode)))))
                   nil)))
    (add-to-list 'bs-configurations conf t)))

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

(use-package newsticker :defer 5
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
  (newsticker-wget-arguments '("-Lkqsm30" "-A\"Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/117.0.0.0 Safari/537.36 Edg/117.0.2045.35\""))
  :config
  (make-directory (file-name-concat newsticker-dir "saved") t)
  (dolist (fn '(newsticker--image-sentinel newsticker--sentinel-work))
    (advice-add fn :around #'my/advice-silence-messages))
  (when (require 'init-net nil t)
    (load "init-rss.el.gpg" t t))
  (when (y-or-n-p-with-timeout "Do you want to run newsticker? " 30 t)
    (newsticker-start t)))

(use-package eww
  :bind
  (:map eww-bookmark-mode-map
        ("n" . next-line)
        ("p" . previous-line)
        ("M-RET" . eww-open-in-new-buffer)
        ("M-n" . eww-next-bookmark)
        ("M-p" . eww-previous-bookmark))
  :custom
  (eww-search-prefix "https://www.mojeek.com/search?newtab=1&cdate=1&qss=DuckDuckGo&date=1&sst=1&arc=none&q=" "https://wiby.org/?q=")
  (eww-auto-rename-buffer 'title)
  (shr-cookie-policy nil)
  (shr-use-xwidgets-for-media t)
  (shr-blocked-images (concat "^https?://" (rx (| "www.baidu.com"))))
  :hook ((eww-bookmark-mode . (lambda () (setq-local goal-column (1+ (/ (window-width) 2)))))))

(use-package browse-url
  :hook ((text-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode))
  :bind
  (:map my/global-prefix-map
        ("/" . 'webjump))
  :custom
  (browse-url-handlers '(("\\`file:" . browse-url-default-browser)))
  :config
  (with-eval-after-load 'webjump
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
                   ("NixPackage" .
                    [simple-query "https://search.nixos.org/packages"
                                  "https://search.nixos.org/packages?from=0&size=50&sort=relevance&type=packages&query=" ""])
                   ("NixOption" .
                    [simple-query "https://search.nixos.org/options"
                                  "https://search.nixos.org/options?from=0&size=50&sort=relevance&type=packages&query=" ""])))
      (add-to-list 'webjump-sites web))))

(use-package tramp
  :bind
  (:map my/global-prefix-map
        ("c" . 'tramp-cleanup-connection)
        ("C" . 'tramp-cleanup-some-buffers))
  :custom
  (tramp-verbose 0)
  (tramp-use-scp-direct-remote-copying t)
  (debug-ignored-errors (cons 'remote-file-error debug-ignored-errors))
  (tramp-use-connection-share t)
  (tramp-ssh-controlmaster-options
   (format "-o ControlPath=%s/ssh-ControlPath-%%r@%%h:%%p -o ControlMaster=auto -o ControlPersist=30m"
           temporary-file-directory))
  :config
  (when my/sys-winnt-p
    (setq tramp-default-method "sshx"
          tramp-use-connection-share nil))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux") "remote-shell"
                     (file-name-concat my/termux-root-directory "usr/bin/bash")))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux") "tmpdir" my/termux-tmp-directory))
  (connection-local-set-profile-variables
   'tramp-connection-local-termux-profile
   `((tramp-remote-path
      . ,(mapcar
          (lambda (x)
            (if (stringp x) (concat my/termux-root-directory x) x))
          (copy-tree tramp-remote-path)))
     (explicit-shell-file-name
      . ,(file-name-concat my/termux-root-directory "usr/bin/bash"))))
  (connection-local-set-profiles
   '(:application tramp :user "termux")
   'tramp-connection-local-termux-profile))

(use-package shell
  :custom
  (shell-completion-execonly nil)
  (shell-completion-fignore '("~" "#" "%"))
  (shell-get-old-input-include-continuation-lines t)
  (shell-kill-buffer-on-exit t))

(use-package sql
  :hook
  (sql-mode
   . (lambda ()
       (mapc
        (lambda (a)
          (setcar (cdr a)
                  (replace-regexp-in-string "\\\\([^?]" "`?\\&" (cadr a))))
        imenu-generic-expression)))
  :bind
  (:map sql-mode-map
        ("C-c C-l s" . my/sql-table-selector)
        ("C-c C-p" . sql-connect))
  (:map sql-interactive-mode-map
        ("C-c C-l s" . my/sql-table-selector))
  :config
  (defun my/sql-table-get-pri-key (sqlbuf table)
    "Get primary key name from table."
    (with-temp-buffer
      (sql-execute-feature sqlbuf (current-buffer) :list-table nil table)
      (goto-char (point-min))
      (re-search-forward "^| \\([[:alnum:]_]+\\).+| PRI |")
      (match-string 1)))
  
  (defun my/sql-table-selector (name &optional arg)
    "Select data from NAME. Default select latest 10 records,
with a positive argument, select latest (* 10 number) records;
with a negative argument, select oldest (* 10 number) records;
with `universal argument', select all records."
    (interactive
     (list (sql-read-table-name "Table name: ")
           current-prefix-arg)
     sql-mode sql-interactive-mode)
    (let ((sqlbuf (sql-find-sqli-buffer))
          (builder (list (format "`%s`" name))))
      (unless sqlbuf
        (user-error "No SQL interactive buffer found"))
      (unless name
        (user-error "No table name specified"))
      (when (or (natnump arg) (listp arg))
        (setq builder
              (append builder
                      `("order by"
                        ,(my/sql-table-get-pri-key sqlbuf name)
                        "desc"))))
      (cond ((and (listp arg)
                  (eq nil (car arg)))
             (setq builder
                   (append builder
                           `("limit" "10"))))
            ((numberp arg)
             (setq builder
                   (append builder
                           `("limit" ,(number-to-string (* (abs arg) 10)))))))
      (sql-execute sqlbuf "SQL table selector"
                   "select * from %s;" nil
                   (mapconcat #'identity builder " "))))
  
  (tempo-define-template
   "my/sql-create-procedure"
   '(%""n
      "DROP PROCEDURE IF EXISTS " (P "Procedure name: " procedure) ";"n
      "DELIMITER //"n
      "CREATE PROCEDURE " (s procedure) "("
      (let ((output '(l)))
        (while-let ((dir (completing-read "Direction: " '("IN" "OUT" "INOUT")))
                    (emptyp (not (string= dir ""))))
          (setq output
                (append output
                        (list dir " " (read-no-blanks-input "Variable name: ") " "
                              (upcase (read-no-blanks-input "Data type: ")) ", "))))
        (if (equal output '(l))
            ", "
          output))
      (delete-char -2) ")"n"COMMENT \""p"\""n
      "BEGIN"n n p n n"END//"n"DELIMITER ;"n)
   nil
   "Drop procedure if exists then create it.")
  (tempo-define-template
   "my/sql-create-function"
   '(%""n
      "DROP FUNCTION IF EXISTS " (P "Function name: " function) ";"n
      "DELIMITER //"n
      "CREATE FUNCTION " (s function) "("
      (let ((output '(l)))
        (while-let ((name (read-no-blanks-input "Variable name: "))
                    (emptyp (not (string= name ""))))
          (setq output
                (append output
                        (list name " "
                              (upcase (read-no-blanks-input "Data type: "))
                              ", "))))
        (if (equal output '(l))
            ", "
          output))
      (delete-char -2) ") RETURNS "
      (let* ((return-var (read-no-blanks-input "Retunrs Variable: "
                                               "data_output"))
             (return-type (upcase (read-no-blanks-input "Retunrs Type: ")))
             (return-default (read-string "Returns Default Value: "))
             (output `(l ,return-type n "COMMENT \"" p "\"" n)))
        (while-let ((act (completing-read "Action: "
                                          '("READS SQL DATA"
                                            "MODIFIES SQL DATA"
                                            "NO SQL")))
                    (emptyp (not (string= act ""))))
          (setq output
                (append output
                        `( ,act n "BEGIN" n "DECLARE " ,return-var
                           " " ,return-type
                           ,@(unless (string= "" return-default)
                               (list " DEFAULT " return-default))
                           ";" n n p n n "RETURN " ,return-var ";"))))
        output)
      n"END//"n"DELIMITER ;"n))
  (tempo-define-template
   "my/sql-if"
   '(%"IF " (P "Contidion: ") " THEN"n "  "p n
      (let ((output '(l)))
        (while-let ((elif (read-string "ELSEIF: "))
                    (emptyp (not (string= elif ""))))
          (setq output (append output `("ELSEIF " ,elif " THEN" n "  "p n))))
        (if (y-or-n-p "ELSE: ")
            (append output `("ELSE" n "  "p n))
          output))
      "END IF;"n))
  (tempo-define-template
   "my/sql-case"
   '(%"CASE"n
      (let ((output '(l)))
        (while-let ((co (read-string "Condition: "))
                    (emptyp (not (string= co ""))))
          (setq output (append output `("  WHEN " ,co " THEN" n> "  "p n))))
        (if (y-or-n-p "ELSE: ")
            (append output `("  ELSE" n "  "p n))
          output))
      "END CASE;"n))
  (define-skeleton my/sql-skeleton-while
    "SQL while statement." "Condition: "
    "WHILE " str " DO"\n "  "_ \n "END WHILE;"\n)
  (define-skeleton my/sql-skeleton-repeat
    "SQL repeat statement." "Condition: "
    "REPEAT"\n "  "_ \n"UNTIL " str \n"END REPEAT;"\n)
  (define-skeleton my/sql-skeleton-loop
    "SQL loop statement, use `LEAVE' or `ITERATE' label." "Label: "
    str & ": " "LOOP" \n "  "_ \n "END LOOP " str ";"\n)
  (define-abbrev-table 'sql-mode-abbrev-table
    '(("proc" #1="" tempo-template-my/sql-create-procedure)
      ("fun" #1# tempo-template-my/sql-create-function)
      ("if" #1# tempo-template-my/sql-if)
      ("case" #1# tempo-template-my/sql-case)
      ("while" #1# my/sql-skeleton-while)
      ("repeat" #1# my/sql-skeleton-repeat)
      ("loop" #1# my/sql-skeleton-loop))))

(use-package sqlite-mode
  :magic ("SQLite format 3\x00" . my/sqlite-view-file-magically))

(use-package dired
  :custom
  (dired-movement-style 'cycle)
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
              ("E" . my/dired-duplicate-file)
              ("f" . my/dired-dwim)
              ("<mouse-2>" . dired-mouse-find-file))
  :config
  (when-let ((7z (or (executable-find "7z")
                     (executable-find "7za")
                     (executable-find "7zz"))))
    (setq archive-7z-program (file-name-base 7z)))
  (setq dired-guess-shell-alist-user
        `(("\\.\\(rar\\|zip\\|7z\\|iso\\)\\(\\.001\\)?\\'"
           ,(format "%s x -aoa" archive-7z-program))
          ("\\.cab\\'" ,(format "%s x -aoa -ocabexpand" archive-7z-program))
          ("\\.apk\\'" "adb install")
          ("\\(?:\\.t\\(?:\\(?:ar\\.\\)?zst\\)\\)\\'"
           "zstd -dc ? | tar -xf -")
          ("\\.\\(mp4\\|mkv\\|avi\\|webm\\|flv\\|m4v\\|mov\\)\\'"
           "ffmpeg -hide_banner -y -strict 2 -hwaccel auto -i ? -vf \"scale='min(2560,iw)':-1\" -c:v hevc_nvenc -rc vbr -cq 19 -qmin 19 -qmax 19 -profile:v main10 -pix_fmt p010le -b:v 0K -bf:v 3 -b_ref_mode middle -temporal-aq 1 -rc-lookahead 32 -c:a libopus -b:a 128k -f mp4 ff-`?`")
          ("\\.\\(png\\|jpe?g\\|gif\\|webp\\|bmp\\)\\'"
           "ffmpeg -hide_banner -y -i ? -vf \"scale='min(4096,iw)':-1\" -c:v libaom-av1 -cpu-used 6 -row-mt 1 -tiles 2x2 -still-picture 1 -crf 20 -f avif ff-`?`")
          (".*" "tar -cf - ? | zstd -o `?`.tzst --long --ultra -9"))))

(use-package dired-aux
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep 't)
  (dired-vc-rename-file t)
  (dired-isearch-filenames 'dwim)
  (dired-compress-file-default-suffix ".zst")
  (dired-compress-directory-default-suffix ".tar.zst")
  :config
  (when my/sys-winnt-p
    (dolist (item `(("\\.exe\\'" .
                     ,(let ((cab (string-replace "/" "\\" (concat temporary-file-directory "cab-" (md5 (system-name))))))
                        (format "makecab %%i %s && copy /b/y \"%s\"+\"%s\" %%o & del /q/f \"%s\""
                                cab (string-replace "/" "\\" (executable-find "extrac32")) cab cab)))))
      (add-to-list 'dired-compress-files-alist item)))
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

(use-package dictionary
  :bind
  (:map my/global-prefix-map
        ("D" . dictionary-search))
  :custom
  (dictionary-server "dict.tw")
  (dictionary-use-single-buffer t))

(use-package epg
  :config
  (unless my/sys-linux-p
    (setq epg-pinentry-mode 'loopback)))

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

(use-package chart
  :commands
  (chart-file-count
   chart-space-usage
   chart-emacs-storage
   chart-emacs-lists
   chart-rmail-from))

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
  (image-dired-external-viewer "ffplay -fs -an -noborder")
  (image-use-external-converter t)
  (doc-view-scale-internally nil)
  (doc-view-resolution 300)
  :config
  (add-to-list 'image-file-name-extensions "avif")
  (unless (executable-find "gm")
    (setq image-dired-cmd-create-thumbnail-program "ffmpeg"
          image-dired-cmd-create-thumbnail-options '("-y" "-i" "%f"
                                                     "-map_metadata" "-1"
                                                     "-vf" "scale=%w:-1"
                                                     "-f" "mjpeg" "%t"))
    (advice-add 'image-dired-create-thumb-1 :around #'my/advice-image-dired-create-thumb-maybe-gs)))

(use-package eglot
  :custom
  (eglot-autoshutdown t)
  (eglot-report-progress nil)
  (eglot-send-changes-idle-time 0.1)
  :config
  (fset #'jsonrpc--log-event #'ignore))

(use-package ediff
  :custom
  (ediff-use-last-dir t)
  (ediff-use-long-help-message t)
  (ediff-show-clashes-only t)
  (ediff-make-buffers-readonly-at-startup t)
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
  (gnus-home-directory (expand-file-name "gnus/" user-emacs-directory)))

(use-package remember
  :custom
  (remember-diary-file (file-name-concat org-directory "remember"))
  :custom
  (add-to-list 'remember-handler-functions 'remember-diary-extract-entries))

(use-package proced
  :bind
  (:map my/global-prefix-map
        ("p" . proced))
  :custom
  (proced-goal-attribute nil)
  (proced-show-remote-processes t)
  (proced-enable-color-flag t))

(use-package pcomplete
  :custom
  (pcomplete-autolist t)
  (pcomplete-recexact t)
  (pcomplete-termination-string "")
  :config
  (when (and (require 'init-pcmpl)
             my/sys-winnt-p)
    (with-eval-after-load 'pcmpl-git
      (advice-add 'pcomplete-from-help :filter-args
                  #'my/advice-pcomplete-from-help))))

(use-package forms)
(use-package ses)
(use-package todo-mode)

(use-package tab-bar :defer 2
  :bind
  (:repeat-map my/tab-bar-history-repeat-map
               ("<right>" . tab-bar-history-forward)
               ("<left>" . tab-bar-history-back))
  :custom
  (tab-bar-select-tab-modifiers '(control))
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-history-limit 100)
  :custom-face
  (tab-bar ((t (:inherit mode-line :box nil))))
  (tab-bar-tab ((t (:inherit mode-line :box t))))
  (tab-bar-tab-inactive ((t (:inherit mode-line-inactive :box nil))))
  :config
  (tab-bar-history-mode))

(use-package saveplace :defer 6
  :config (save-place-mode))

(use-package recentf :defer 1
  :bind
  (:map my/global-prefix-map
        ("r" . 'recentf))
  :config
  (recentf-mode)
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/data/data/com\\.termux/files/home/tmp" "/tmp/" "/ssh:"
                     "/sshx:" ,(file-name-concat package-user-dir ".*-autoloads\\.el\\'"))))

(use-package esh-mode
  :bind
  (:map my/global-prefix-map
        ("e" . eshell))
  (:repeat-map eshell-command-repeat-map
               ("b" . eshell-backward-argument)
               ("f" . eshell-forward-argument))
  (:repeat-map eshell-prompt-repeat-map
               ("n" . eshell-next-prompt)
               ("p" . eshell-previous-prompt)
               :exit
               ("m" . eshell-copy-old-input))
  :custom
  (eshell-prefer-lisp-functions t)
  (eshell-default-target-is-dot t)
  (eshell-mv-overwrite-files nil)
  (eshell-cp-overwrite-files nil)
  (eshell-cp-interactive-query t)
  (eshell-rm-interactive-query t)
  (eshell-rm-removes-directories t)
  (eshell-pushd-dunique t)
  (eshell-pushd-dextract t)
  (eshell-scroll-to-bottom-on-output 'others)
  :config
  (keymap-unset eshell-command-repeat-map "C-b")
  (keymap-unset eshell-command-repeat-map "C-f")
  (keymap-unset eshell-prompt-repeat-map "C-n")
  (keymap-unset eshell-prompt-repeat-map "C-p")
  (add-hook 'eshell-expand-input-functions #'eshell-expand-history-references)
  (dolist (mod '(eshell-smart eshell-elecslash eshell-tramp eshell-xtra))
    (add-to-list 'eshell-modules-list mod)))

(use-package comint
  :bind
  (:map comint-mode-map
        ("SPC" . comint-magic-space)
        ("C-c M-n" . comint-next-input)
        ("C-c M-p" . comint-previous-input))
  (:repeat-map comint-input-repeat-map
               ("p" . comint-previous-input)
               ("n" . comint-next-input))
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
  :unless my/sys-android-p
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
  :unless my/sys-android-p
  :config
  (shadow-initialize))

(use-package treesit :defer 1
  :after prog-mode
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
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
  (when (require 'init-prog nil t)
    (my/ts-mode-enable)))

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
  ;; (org-replace-disputed-keys t "see `'org-disputed-keys'")
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-cycle-emulate-tab 'whitestart)
  (org-use-speed-commands t)
  (org-goto-auto-isearch nil)
  (org-goto-interface 'outline-path-completion)
  (org-default-notes-file (file-name-concat org-directory "inbox.org"))
  (org-refile-use-cache nil)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archive")
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
  :config (add-to-list 'org-modules 'id))

(use-package org-list
  :custom
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-list-use-circular-motion t))

(use-package org-table
  ;; :hook (text-mode . turn-on-orgtbl)
  :custom
  ;; (org-table-header-line-p t)
  (org-table-use-standard-references t)
  (org-table-automatic-realign nil))

(use-package org-agenda
  :bind
  (:map global-map
        ("C-c a" . org-agenda))
  :custom
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-inhibit-startup t)
  ;; (org-agenda-use-tag-inheritance nil)
  ;; (org-agenda-ignore-properties '(effort appt stats category))
  (org-agenda-files `(,org-default-notes-file
                      ,(file-name-concat org-directory "todo")))
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
  (:map my/global-prefix-map
        ("l" . org-store-link)
        ("L" . org-insert-link-global))
  :custom
  (org-id-link-to-org-use-id 'create-if-interactive)
  (org-link-use-indirect-buffer-for-internals t)
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

(use-package dired-duplicates
  :if (package-installed-p 'dired-duplicates))

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
  (my/get-bt-tracker "https://gitea.com/XIU2/TrackersListCollection/raw/branch/master/best_aria2.txt")
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

(use-package nov
  :if (package-installed-p 'nov)
  :mode ("\\.epub\\'" . nov-mode)
  :custom
  (nov-unzip-program archive-7z-program)
  (nov-unzip-args '("x" filename))
  :config
  ;; advice for 7z cli cannot split "-o" and `direcotry'
  (advice-add 'nov-unzip-epub :around
              (lambda (orig-fun &rest args)
                (let ((nov-unzip-args
                       (append nov-unzip-args
                               (list (format "-o\"%s\"" (car args))))))
                  (apply orig-fun args)))))

(use-package fdroid
  :if (package-installed-p 'fdroid)
  :vc (:url "https://github.com/migalmoreno/fdroid.el"))

(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here

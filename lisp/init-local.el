;;; init-local.el --- Personal Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(cond ((eq system-type 'windows-nt)
       (require 'init-winnt)
       (require 'init-desk))
      ((eq system-type 'gnu/linux)
       (require 'init-linux)
       (require 'init-desk))
      ((eq system-type 'android) (require 'init-android)))

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

(use-package newsticker :defer 60
  :custom
  (newsticker-obsolete-item-max-age 864000)
  (newsticker-treeview-date-format "%y.%m.%d, %H:%M")
  (newsticker-url-list-defaults nil)
  (newsticker-automatically-mark-items-as-old nil)
  (newsticker-hide-old-items-in-newsticker-buffer t "plainview only")
  (newsticker-retrieval-interval 1800)
  (newsticker-retrieval-method 'extern)
  :init (load "init-privacy.el.gpg" t t)
  :config
  (defun newsticker-treeview-eww-browse-url ()
    "Call `eww-browse-url' for the link of the item at point."
    (interactive)
    (with-current-buffer (newsticker--treeview-list-buffer)
      (let ((url (get-text-property (point) :nt-link)))
	(when url
          (eww-browse-url url)
          (if newsticker-automatically-mark-visited-items-as-old
              (newsticker-treeview-mark-item-old))
	  (switch-to-buffer-other-window "*eww*" t)
	  ))))
  (define-key newsticker-treeview-mode-map "e" 'newsticker-treeview-eww-browse-url)

  (defun my/newsticker-start (&optional _do-not-complain-if-running)
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
  (advice-add 'newsticker-start :override #'my/newsticker-start)
  (defun my/newsticker--image-sentinel (process _event)
    (let* ((p-status (process-status process))
           (exit-status (process-exit-status process))
           (feed-name (process-get process 'nt-feed-name))
           (directory (process-get process 'nt-directory))
           (filename (process-get process 'nt-filename)))
      (catch 'oops
        (unless (and (eq p-status 'exit)
                     (= exit-status 0))
          (newsticker--image-remove directory feed-name)
          (throw 'oops nil))
        (newsticker--image-save (process-buffer process) directory filename))))
  ;; remove message: "Error while retrieving image"
  (advice-add 'newsticker--image-sentinel :override #'my/newsticker--image-sentinel)
  (newsticker-start t)
  )

(use-package eww
  :custom
  (url-cookie-trusted-urls '())
  (url-cookie-untrusted-urls '(".*"))
  (shr-inhibit-images t "images maybe hang in newsticker")
  :hook
  (eww-mode . (lambda ()
                (setq-local shr-inhibit-images nil))))

(use-package tramp
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
   '(:application tramp :machine "termux-mi")
   'tramp-connection-local-termux-profile)
  )

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-l")
  (delete-by-moving-to-trash t)
  (wdired-allow-to-change-permissions 'advanced)
  (wdired-use-interactive-rename t)
  :bind (:map dired-mode-map
              ("× μ" . my/mpv-image)
              ("C-c d" . my/dired-duplicate-file)
              ("e" . my/dired-process))
  :config
  (defun my/dired-process ()
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
    "Duplicate the current file in Dired."
    (interactive "p")
    (let ((filename (dired-get-filename)))
      (setq target (concat (file-name-sans-extension filename)
                           "-old"
                           (if (> arg 1) (number-to-string arg))
                           (file-name-extension filename t)))
      (if (file-directory-p filename)
          (copy-directory filename target)
        (copy-file filename target))
      )
    )
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.\\(rar\\|zip\\|7z\\)\\(\\.001\\)?$"
               (concat archive-7z-program " x -aoa")))))

(use-package dired-aux
  :config
  (push `("\\.\\(zip\\|7z\\)\\'" . ,(concat archive-7z-program " a -mx5 %o %i"))
        dired-compress-files-alist)
  (setq dired-compress-file-suffixes
        (append `(("\\.\\(zip\\|rar\\)\\'" #1=""
                   ,(concat archive-7z-program " x -aoa -o%o %i"))
                  ("\\.t\\(ar\\.\\)?\\(gz\\|xz\\|bz\\)\\'" #1#
                   ,(concat archive-7z-program " x %i -so | "
                            archive-7z-program " x -aoa -si -ttar -o%o"))
                  ("\\.t\\(ar\\.\\)?zst\\'" #1# "zstd -dc %i | tar -xf -")
                  ("\\.zst\\'" #1# "zstd -d --rm"))
                dired-compress-file-suffixes)))

(use-package dictionary
  :bind
  ("C-c k d" . dictionary-search)
  :custom
  (dictionary-server "dict.tw")
  (dictionary-use-single-buffer t))

(use-package org
  :hook
  ((org-mode . visual-line-mode)  ; wrap lines at word breaks
   (org-mode . flyspell-mode)) ; spell checking!
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
  (org-use-speed-commands t)
  (org-directory "~/org/")
  (org-default-notes-file "~/org/inbox.org")
  (org-agenda-files `(,org-default-notes-file
                      "~/org/todo"))
  (org-refile-use-cache nil)
  (org-archive-mark-done nil)
  (org-archive-location "%s_archive::* Archive")
  (ispell-program-name "aspell")
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

  (require 'oc-csl)                     ; citation support

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

(use-package calendar
  :custom
  (diary-file "~/org/diary"))

(use-package image
  :custom
  (image-use-external-converter t)
  (image-dired-cmd-create-thumbnail-program "ffmpeg")
  (image-dired-cmd-create-thumbnail-options '("-i" "%f"
                                              "-map_metadata" "-1"
                                              "-vf" "scale=%w:-1"
                                              "-f" "mjpeg" "%t"))
  :config
  (add-to-list 'image-file-name-extensions "avif"))

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

(use-package emacs
  :init
  (setq-default truncate-lines t
		system-time-locale "C")
  (fset 'yes-or-no-p 'y-or-n-p)
  :hook
  ((prog-mode . electric-pair-local-mode))
  :bind
  (([remap eval-expression] . pp-eval-expression)
   ([remap eval-last-sexp] . pp-eval-last-sexp))
  :custom
  (help-window-select t "Switch to help buffers automatically")
  (bookmark-save-flag 1)
  ;; (mouse-1-click-follows-link -450 "click set point, long press do action")
  (reb-re-syntax 'string)
  (sentence-end-double-space nil)
  (desktop-auto-save-timeout 600)
  (eshell-scroll-to-bottom-on-input 'this)
  (comint-scroll-to-bottom-on-input 'this)
  (comint-scroll-to-bottom-on-output t)
  :config
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (javascript-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (winner-mode)
  (desktop-save-mode 1)
  (prefer-coding-system 'utf-8)
  (scroll-bar-mode -1)
  )

(use-package gnus
  :custom
  (gnus-home-directory "~/.emacs.d/gnus/")
  )

;; tab-bar
(use-package tab-bar
  :custom
  (tab-bar-history-mode t)
  (tab-bar-select-tab-modifiers '(meta))
  :bind (("C-x C-<left>" . tab-bar-history-back)
	 ("C-x C-<right>" . tab-bar-history-forward))
  )

;; switch frame
(global-set-key (kbd "A-q") (lambda () (interactive) (other-frame 1)))
(global-set-key (kbd "A-e") (lambda () (interactive) (other-frame -1)))

(use-package recentf
  :custom
  (recentf-max-saved-items 1000)
  (recentf-exclude `("/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'"))))

(package-initialize)

(use-package treesit
  :custom
  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (sql "https://github.com/DerekStride/tree-sitter-sql" "gh-pages")
     ;; ini
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (nix "https://github.com/nix-community/tree-sitter-nix")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     ;; frontend
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
     ))

  :config
  (use-package nix-ts-mode
    :if (and
	 (package-installed-p 'nix-ts-mode)
	 (treesit-language-available-p 'nix))
    :mode "\\.nix\\'")

  ;; (package-vc-install "https://github.com/mickeynp/combobulate")
  ;; (package-vc-install "https://github.com/mickeynp/html-ts-mode")
  (use-package combobulate
    :if (package-installed-p 'combobulate)
    :preface
    (setq combobulate-key-prefix "C-c m")
    :hook
    ((python-ts-mode . combobulate-mode)
     (js-ts-mode . combobulate-mode)
     (css-ts-mode . combobulate-mode)
     (yaml-ts-mode . combobulate-mode)
     (json-ts-mode . combobulate-mode)
     (typescript-ts-mode . combobulate-mode)
     (tsx-ts-mode . combobulate-mode))
    :config
    (when (package-installed-p 'html-ts-mode)
      (add-to-list 'major-mode-remap-alist '(mhtml-mode . html-ts-mode))
      (add-hook 'html-ts-mode-hook 'combobulate-mode)
      )
    )
  )

(use-package which-key
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
  :init
  (require 'corfu)
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

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


(use-package org-anki
  :if (package-installed-p 'org-anki)
  :custom (org-anki-default-deck "org-anki"))

(use-package ox-pandoc
  :if (package-installed-p 'ox-pandoc)
  :after (org))

(provide 'init-local)
;;; init-local.el ends here

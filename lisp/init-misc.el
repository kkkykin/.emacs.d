;;; init-misc.el --- Misc Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defvar-keymap my/global-prefix-map
  :doc "A keymap for myself."
  "/" #'webjump
  "a" #'area2-download-list
  "b" #'speedbar
  "c" #'org-clock-in-last
  "C" #'org-clock-in
  "D" #'dictionary-search
  "e" #'eshell
  "l" #'org-store-link
  "L" #'org-insert-link-global
  "o" #'org-clock-out
  "O" #'org-clock-goto
  "P" #'proced
  "r" #'recentf
  "s" #'scratch-buffer
  "t" #'tramp-cleanup-connection
  "T" #'tramp-cleanup-some-buffers)
(keymap-set ctl-x-map "j" my/global-prefix-map)

(defvar-keymap my/mpc-prefix-map
  :doc "A keymap for mpc."
  "s" #'mpc-toggle-play
  "n" #'mpc-next
  "p" #'mpc-prev
  "b" #'mpc-status-buffer-show
  "g" #'mpc-seek-current
  "r" #'mpc-toggle-repeat
  "z" #'mpc-toggle-shuffle
  "a" #'mpc-toggle-single
  "u" #'mpc-update)
(keymap-set ctl-x-map "c" my/mpc-prefix-map)


;; theme

(defcustom my/all-theme-list (cons 'default (custom-available-themes))
  "list of Custom themes available for loading."
  :group 'my
  :type '(repeat symbol))

(defcustom my/light-theme-list
  (cl-intersection
   my/all-theme-list
   '( adwaita default dichromacy leuven modus-operandi-deuteranopia
      modus-operandi modus-operandi-tinted modus-operandi-tritanopia
      tango tsdh-light whiteboard))
  "Built-in light themes."
  :group 'my
  :type '(repeat symbol))

(defcustom my/dark-theme-list
  (cl-intersection
   my/all-theme-list
   '( deeper-blue leuven-dark manoj-dark misterioso
      modus-vivendi-deuteranopia modus-vivendi
      modus-vivendi-tinted modus-vivendi-tritanopia
      tango-dark tsdh-dark wheatgrass wombat))
  "Built-in dark themes."
  :group 'my
  :type '(repeat symbol))

;; (defcustom my/fonts-list
;;   '(("霞鹜文楷等宽" . #1=(198 108 140 "https://github.com/lxgw/LxgwWenKai/releases"))
;;     ("LXGW WenKai Mono" . #1#)
;;     ("等距更纱黑体 SC" . #2=(188 108 130 "https://github.com/be5invis/Sarasa-Gothic/releases"))
;;     ("Sarasa Mono SC" . #2#)
;;     ("Unifont-JP" . (198 108 142 "https://unifoundry.com/unifont/index.html"))
;;     ("UnifontExMono" . (198 108 142 "https://github.com/stgiga/UnifontEX/releases")))
;;   "Fonts. Heights. Source."
;;   :group 'my
;;   :type '(repeat string))

;; (defun my/setup-faces ()
;;   "Randomize setup faces."
;;   (when (display-graphic-p)
;;     (when-let ((fonts
;;                 (cl-intersection
;;                  (font-family-list)
;;                  (mapcar #'car my/fonts-list) :test 'equal)))
;;       (let ((font (assoc (seq-random-elt fonts) my/fonts-list)))
;;         (set-face-attribute 'default nil :font (car font) :height
;;                             (cond ((< (display-pixel-width) 1920) (cadr font))
;;                                   ((> (display-pixel-width) 1920) (cadddr font))
;;                                   (t (caddr font))))))
;;     (my/shuffle-set-theme (if (my/system-dark-mode-enabled-p)
;;                               my/dark-theme-list
;;                             my/light-theme-list))))

(defcustom my/fonts-list
  '(("LXGW WenKai Mono" (33 14 . 23) "https://github.com/lxgw/LxgwWenKai/releases")
    ("Sarasa Mono SC" (32 14 . 22) "https://github.com/be5invis/Sarasa-Gothic/releases")
    ;; ("Unifont-JP" #1=(33 14 . 24) "https://unifoundry.com/unifont/index.html")
    ;; ("UnifontExMono" #1# "https://github.com/stgiga/UnifontEX/releases")
    )
  "Fonts. Heights. Source."
  :group 'my
  :type '(repeat string))

(defun my/system-dark-mode-enabled-p ()
  "Check if dark-mode is enabled. ref:
https://github.com/LionyxML/auto-dark-emacs/blob/master/auto-dark.el"
  (pcase system-type
    ('windows-nt
     (eq 0 (w32-read-registry 'HKCU "SOFTWARE/Microsoft/Windows/CurrentVersion/Themes/Personalize" "AppsUseLightTheme")))
    ('gnu/linux
     (eq 1 (caar (dbus-ignore-errors
                   (dbus-call-method
                    :session
                    "org.freedesktop.portal.Desktop"
                    "/org/freedesktop/portal/desktop"
                    "org.freedesktop.portal.Settings" "Read"
                    "org.freedesktop.appearance" "color-scheme")))))
    (_ nil)))

(defun my/set-theme (theme)
  "Set one theme only."
  (if (eq theme 'default)
      (dolist (item custom-enabled-themes)
        (disable-theme item))
    (unless (memq theme custom-known-themes)
      (load-theme theme t t))
    (pcase theme
      ('adwaita
       (custom-theme-set-faces theme '(hl-line ((t (:extend t :background "navajo white"))))))
      ('whiteboard
       (custom-theme-set-faces theme '(hl-line ((t (:extend t :background "wheat"))))))
      ('tango
       (custom-theme-set-faces theme '(hl-line ((t (:extend t :background "cornsilk")))))))
    (dolist (item custom-enabled-themes)
      (disable-theme item))
    (enable-theme theme)))

(defun my/shuffle-set-theme (&optional themes)
  "Shuffle set theme."
  (interactive "P")
  (let* ((themes (pcase themes
                   ('nil (if (my/system-dark-mode-enabled-p)
                             my/dark-theme-list
                           my/light-theme-list))
                   ((guard (lambda (a) (consp arg)))
                    (if (my/system-dark-mode-enabled-p)
                        my/light-theme-list
                      my/dark-theme-list))
                   (_ themes)))
         (theme (seq-random-elt themes)))
    (my/set-theme theme)
    (message "Current theme: %s" (symbol-name theme))))

(defun my/setup-faces ()
  "Randomize setup faces."
  (when (display-graphic-p)
    (when-let
        ((fonts
          (delq nil
                (mapcar (lambda (a)
                          (let ((b (font-spec :name
                                              (format "%s:pixelsize=%d" (car a)
                                                      (pcase (display-pixel-width)
                                                        ((pred (> 1920)) (caadr a))
                                                        ((pred (< 1920)) (cddadr a))
                                                        (_ (cadadr a)))))))
                            (and (find-font b) b)))
                        my/fonts-list))))
      (set-face-attribute 'default nil :font (seq-random-elt fonts)))
    (my/shuffle-set-theme)))
(add-hook 'window-setup-hook #'my/setup-faces)

(defun my/set-font-current-buffer (&optional font)
  "Set font for current buffer."
  (interactive "P")
  (let ((fonts (pcase font
                 ((pred stringp) (list font))
                 ('nil '("Unifont-JP" "UnifontExMono"))
                 ((pred listp) (list (completing-read
                                      "Fonts: " (font-family-list)))))))
    (when-let ((font (cl-intersection
                      (font-family-list)
                      fonts :test 'equal)))
      (face-remap-add-relative 'default :family (car font)))))


;; termux

(defcustom my/termux-root-directory "/data/data/com.termux/files/"
  "Andriod termux root path."
  :group 'my
  :type 'directory)

(defcustom my/termux-tmp-directory (file-name-concat my/termux-root-directory "home/tmp/")
  "Android termux tmp path."
  :group 'my
  :type 'directory)

(with-eval-after-load 'tramp
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
   ;; FIXME: If the username is not explicitly specified when accessing
   ;; a remote host, the :user option does not work. Therefore, remember
   ;; to use the 'sshx' method when accessing Termux, and 'scpx' otherwise.
   '(:application tramp :protocol "sshx" :user "t")
   'tramp-connection-local-termux-profile))


;; bookmark

(defcustom my/bookmark-shared-prefix "s/"
  "Prefix of shared bookmark name."
  :group 'my
  :type 'string)

(defcustom my/bookmark-shared-file (expand-file-name "bookmark-share" user-emacs-directory)
  "Shared bookmark file cross device."
  :group 'my
  :type 'file)
(when (file-exists-p my/bookmark-shared-file)
  (bookmark-load my/bookmark-shared-file nil t))

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
      (when-let ((sorted (seq-sort-by #'car #'string< new-shared))
                 (need-update-p (not (equal sorted ori-shared)))
                 (bookmark-alist sorted))
        (funcall orig-fun nil my/bookmark-shared-file nil))
      (let ((bookmark-alist new-local))
        (apply orig-fun args)))))
(advice-add 'bookmark-save :around 'my/advice-bookmark-save)

(defun my/advice-silence-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN.
https://scripter.co/using-emacs-advice-to-silence-messages-from-functions"
  (let ((inhibit-message t)    ;Don't show the messages in Echo area
        (message-log-max nil)) ;Don't show the messages in the *Messages* buffer
    (apply orig-fun args)))

(defun my/rclone-quit ()
  "Exit rclone safely."
  (interactive)
  (when-let* ((args '("rc" "core/quit"))
              (auth (car (auth-source-search :max 1 :host "rclone.localhost")))
              (args (append args `(,(format "--rc-user=%s" (plist-get auth :user))
                                   ,(format "--rc-pass=%s" (auth-info-password auth))))))
    (apply #'start-process "rclone-quit" nil "rclone" args)))

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


;; dired

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

(defun my/dired-goto-random-file ()
  "Goto random file in current-dir."
  (interactive nil dired-mode)
  (dired-goto-file
   (seq-random-elt (directory-files (dired-current-directory) t "^[^.]" t))))

(defun my/advice-image-dired-create-thumb-maybe-gs (oldfun &rest args)
  (if (string= (file-name-extension (car args)) "pdf")
      (let ((image-dired-cmd-create-thumbnail-program "gs")
            (image-dired-cmd-create-thumbnail-options
             '("-sDEVICE=jpeg" "-dSAFER" "-r20" "-o" "%t" "%f")))
        (apply oldfun args))
    (apply oldfun args)))

(with-eval-after-load 'dired
  (define-keymap :keymap dired-mode-map
    ;; z f available
    "SPC" nil
    "SPC d" #'my/dired-duplicate-file
    "SPC R" #'my/dired-goto-random-file))

(with-eval-after-load 'image-dired
  (unless (executable-find "gm")
    (setq image-dired-cmd-create-thumbnail-program "ffmpeg"
          image-dired-cmd-create-thumbnail-options '("-y" "-i" "%f"
                                                     "-map_metadata" "-1"
                                                     "-vf" "scale=%w:-1"
                                                     "-f" "mjpeg" "%t"))
    (advice-add 'image-dired-create-thumb-1 :around #'my/advice-image-dired-create-thumb-maybe-gs)))


;; speedbar
(defun my/speedbar-show-unknown-files ()
  "Temporary show unknown files."
  (interactive)
  (let ((speedbar-show-unknown-files t))
    (speedbar-refresh)))

(defun my/speedbar-item-diff ()
  "Diff the item under the cursor or mouse with
`speedbar-last-selected-file'."
  (interactive)
  (if-let* ((f (speedbar-line-file))
            (filep (file-regular-p f)))
      (progn
        (diff f speedbar-last-selected-file)
        (dframe-close-frame))
    (error "Not a file")))

(with-eval-after-load 'speedbar
  (define-keymap
    :keymap speedbar-file-key-map
    "=" #'my/speedbar-item-diff
    "(" #'my/speedbar-show-unknown-files))


;; find & grep

(with-eval-after-load 'find-dired
  (when (string= find-program "fd")
    (define-advice find-dired-with-command (:filter-args (args) maybe-fd)
      "When use `fd' program instead of `find'."
      (let ((cmd (cadr args)))
        (cond
         ((string-prefix-p "fd . \"(\"" cmd)
          (setcdr
           args
           `(,(cond
               ((string-prefix-p "fd . \"(\" -type f -exec " cmd)
                (if (eq system-type 'windows-nt)
                    (replace-regexp-in-string "fd \\. \\\"(\\\" -type f -exec ug -q \\(.+?\\) \\\"{}\\\" \\\";\\\".+-X \\(.+\\) {}.+"
                                              "ug -l \\1 * | sed \"s$\\\\\\\\$/$g\" | xargs \\2" cmd)
                  (replace-regexp-in-string "fd \\(\\(?:.+\\) \\(?:\\(?:\\\")\\\"\\)\\|)\\)\\) -X \\(.+\\)"
                                            "find \\1 -exec \\2" cmd)))
               (t
                (replace-regexp-in-string " \\. \\\"(\\\"\\(.+\\)\\\")\\\""
                                          "\\1" cmd))))
           ))))
      args)
    (setq find-name-arg "-g"
          find-ls-option '("-X ls -ldh {} ;" . "-ldh"))))

(with-eval-after-load 'grep
  (defcustom ug-fd-command
    '("fd -t f -X ug --color=auto -nH --null -e \"\" {} ;" . 43)
    "`ug-fd' instead of `grep-find'")
  (defcustom ug-fd-template
    "fd --base-directory <D> -t f <X> <F> -X ug <C> -nH --null -e <R> {} ;"
    "`ug-fd' instead of `grep-find'.")
  (when (and (or (string= grep-program "ug")
                 (executable-find "up"))
             (or (string= find-program "fd")
                 (executable-find "fd")))
    (setq grep-find-command ug-fd-command
          grep-find-template ug-fd-template))

  (when (string= find-program "fd")
    (define-advice rgrep-default-command (:around (oldfun re files dir) maybe-fd)
      "Use `fd' syntax."
      (if (string-prefix-p "fd " grep-find-template)
          (let ((grep-find-template
                 (string-replace
                  "<X> <F>"
                  (concat
                   (shell-quote-argument
                    (mapconcat
                     (lambda (a)
                       (format "(^%s$)"
                               (mapconcat
                                (lambda (b)
                                  (pcase b (?* ".*") (?? ".")
                                         (_ (regexp-quote (string b)))))
                                a)))
                     (split-string files) "|")
                    grep-quoting-style)
                   " "
                   (when grep-find-ignored-directories
                     (mapconcat
                      (lambda (d)
                        (concat "-E " (shell-quote-argument (concat "*/" d)
                                                            grep-quoting-style)))
                      (rgrep-find-ignored-directories dir)
                      " "))
                   " "
                   (when grep-find-ignored-files
                     (mapconcat
                      (lambda (ignore)
                        (cond
                         ((stringp ignore)
                          (concat "-E " (shell-quote-argument
                                         ignore grep-quoting-style)))
                         ((consp ignore)
                          (when (funcall (car ignore) dir)
                            (concat "-E " (shell-quote-argument
                                           (cdr ignore) grep-quoting-style))))))
                      grep-find-ignored-files " ")))
                  grep-find-template)))
            (funcall oldfun re files dir))
        (funcall oldfun re files dir)))))


;; re-builder

(defun my/reb-copy-match (&optional group)
  "Copy current match strings into the `kill-ring'. With
`universal-argument' select nth group. Default copy first group."
  (interactive "p" reb-mode)
  (with-current-buffer reb-target-buffer
    (kill-new
     (mapconcat
      (lambda (a)
        (when (equal (overlay-get a 'priority) group)
          (format "%s
" (buffer-substring-no-properties (overlay-start a) (overlay-end a)))))
      (reverse reb-overlays)))))
(with-eval-after-load 're-builder
  (keymap-set reb-mode-map "C-c M-w" 'my/reb-copy-match))


;; isearch

(with-eval-after-load 'isearch
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

(defun isearch-other-window (regexp-p)
    "Function to isearch-forward in the next window.
With prefix arg REGEXP-P, perform a regular expression search.
ref: https://karthinks.com/software/emacs-window-management-almanac/"
    (interactive "P")
    (unless (one-window-p)
      (with-selected-window (other-window-for-scrolling)
        (isearch-forward regexp-p))))

(keymap-global-set "C-M-s" #'isearch-other-window)


;; tab-line

(defcustom my/tab-line-excluded-buffer-list
  `(,(rx (| "*Async-native-compile-log*"
            "*Pp Eval Output*")))
  "Buffer which never show in tab-line.")

(defun my/tab-line-tabs-buffer-group-by-mode-exclude-some-buffer
    (&optional buffer)
  "Exclude some buffer and group by mode."
  (when-let* ((buf (or buffer (current-buffer)))
              (exclude-p (cl-find-if-not
                          (lambda (a) (buffer-match-p a buf))
                          my/tab-line-excluded-buffer-list)))
    (if (boundp 'tab-line-tabs-buffer-group-by-mode)
        (funcall 'tab-line-tabs-buffer-group-by-mode buf)
      (let ((tab-line-tabs-buffer-group-function nil))
        (funcall 'tab-line-tabs-buffer-group-name buf)))))

(setq tab-line-tabs-buffer-group-function
      #'my/tab-line-tabs-buffer-group-by-mode-exclude-some-buffer)


;; window

(defun my/select-side-window (&optional side n frame)
  "Select nth side window."
  (if-let* ((windows (window-at-side-list frame side))
            (window (or (nth (1- n) windows) (car (last windows)))))
      (select-window window)
    (user-error "No window here.")))
(define-keymap :keymap ctl-x-map
  "C-<up>" (lambda (&optional n) (interactive "p") (my/select-side-window 'top n))
  "C-<down>" (lambda (&optional n) (interactive "p") (my/select-side-window 'bottom n))
  "C-<left>" (lambda (&optional n) (interactive "p") (my/select-side-window 'left n))
  "C-<right>" (lambda (&optional n) (interactive "p") (my/select-side-window 'right n)))


;; repeat

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
          (_ (eval-defun)))))


;; file

(defun my/file-modified-recently-p (file seconds)
  "Check file is modified recently."
  (and (file-exists-p file)
       (time-less-p
        (current-time)
        (time-add
         (file-attribute-modification-time
          (file-attributes file))
         seconds))))


;; etc

(defun my/generate-uuid (&optional obj)
  "Generate UUID format string."
  (interactive)
  (if obj
      (let ((xstr (md5 obj)))
        (format "%s-%s-4%s-%s-%s"
                (substring xstr 0 8)
                (substring xstr 8 12)
                (substring xstr 12 15)
                (substring xstr 16 20)
                (substring xstr 20 32)))
    (let ((org-id-method 'uuid))
      (org-id-new))))


;; pass
(defun my/generate-pass (&optional arg)
  "Generate password then copy to `kill-ring'. If call with
`universal-argument' then insert into buffer instead of copy."
  (interactive "P")
  (let ((pass "")
        (xc-program "keepassxc-cli")
        (gpg-program "gpg")
        (ssl-program "openssl")
        (alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (setq pass
          (cond
           ((executable-find xc-program)
            (car (process-lines xc-program "generate" "-lUnL15")))
           ((executable-find gpg-program)
            (car (process-lines gpg-program "--gen-random" "--armor" "1" "12")))
           ((executable-find ssl-program)
            (car (process-lines ssl-program "rand" "-base64" "12")))
           (t (while-let ((enough (< (length pass) 15))
                          (i (% (abs (random)) (length alnum))))
                (setq pass (concat pass (substring alnum i (1+ i))))))))
    (if arg
        (insert pass)
      (kill-new pass))))


;; tools

(defun my/pure-pure-pure-url (url)
  "Remove invalid char in url."
  (interactive (list (read-string "Url: " nil nil (current-kill 0 t))))
  (kill-new (replace-regexp-in-string
             (concat "[^" url-get-url-filename-chars "]+") "" url)))

(provide 'init-misc)
;;; init-misc.el ends here

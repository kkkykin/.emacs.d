;;; init-misc.el --- Misc Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(defcustom my/light-theme-list
  '( adwaita dichromacy leuven modus-operandi-deuteranopia modus-operandi
     modus-operandi-tinted modus-operandi-tritanopia tango
     tsdh-light whiteboard)
  "Built-in light themes."
  :group 'my
  :type '(repeat symbol))

(defcustom my/dark-theme-list
  '( deeper-blue leuven-dark manoj-dark misterioso
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
   '(:application tramp :user "termux")
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

(defun my/system-dark-mode-enabled-p ()
  "Check if dark-mode is enabled."
  (pcase system-type
    ('windows-nt
     (string-search "0x1" (shell-command-to-string "reg query HKCU\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize /v AppsUseLightTheme")))))

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
(add-hook 'window-setup-hook #'my/setup-faces)

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

(defun my/dired-dwim ()
  "start process with current file"
  (interactive)
  (let ((filename (dired-get-filename nil t)))
    (when (eq filename nil)
      (setq filename default-directory))
    (my/mpv-intent filename))
  )

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
  (if (string= (file-name-extension (car args)) "pdf")
      (let ((image-dired-cmd-create-thumbnail-program "gs")
            (image-dired-cmd-create-thumbnail-options
             '("-sDEVICE=jpeg" "-dSAFER" "-r20" "-o" "%t" "%f")))
        (apply oldfun args))
    (apply oldfun args)))

(with-eval-after-load 'dired
  (define-keymap :keymap dired-mode-map
    "E" #'my/dired-duplicate-file
    "f" #'my/dired-dwim))

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
(defun my/advice-find-dired-with-command-maybe-fd (args)
  "Use `fd' instead of `find', fix some syntax error."
  (when-let* ((cmd (cadr args))
              (fix-p (string-prefix-p "fd . \"(\"" cmd)))
    (setcdr args
            `(,(replace-regexp-in-string " \\. \\\"(\\\"\\(.+\\)\\\")\\\""
                                         "\\1" cmd))))
  args)
(with-eval-after-load 'find-grep
  (when (string= find-program "fd")
    (advice-add 'find-dired-with-command :filter-args #'my/advice-find-dired-with-command-maybe-fd)
    (setq find-name-arg "-g"
          find-ls-option '("-X ls -ldh {} ;" . "-ldh"))))

(defun my/advice-rgrep-default-command-maybe-fd (oldfun &rest args)
  "Use `fd' synyax."
  (if (string-prefix-p "fd " grep-find-template)
      (let* ((files (cadr args))
             (dir (caddr args))
             (grep-find-template
              (string-replace
               "<F>"
               (shell-quote-argument
                (concat ".*\\.("
                        (mapconcat
                         (lambda (x) (string-replace "*." "" x))
                         (split-string files) "|")
                        ")$")
                grep-quoting-style)
               grep-find-template))
             (grep-find-template
              (string-replace
               "<X>"
               (concat
                (and grep-find-ignored-directories
                     (concat
                      (mapconcat
                       (lambda (d) (concat "-E " (shell-quote-argument
                                              (concat "*/" d)
                                              grep-quoting-style)))
                       (rgrep-find-ignored-directories dir)
                       " ")
                      " "))
                (and grep-find-ignored-files
                     (concat
                      (mapconcat
                       (lambda (ignore)
                         (cond ((stringp ignore)
                                (concat "-E " (shell-quote-argument
                                               (concat "*." ignore)
                                               grep-quoting-style)))
                               ((consp ignore)
                                (and (funcall (car ignore) dir)
                                     (concat "-E "
                                             (shell-quote-argument
                                              (concat "*." (cdr ignore))
                                              grep-quoting-style))))))
                       grep-find-ignored-files
                       " ")
                      " ")))
               grep-find-template)))
        (apply oldfun args))
    (apply oldfun args)))

(with-eval-after-load 'grep
  (when (string= find-program "fd")
    (advice-add 'rgrep-default-command :around
                #'my/advice-rgrep-default-command-maybe-fd)))


;; re-builder

(defun my/reb-copy-match (&optional priority)
  "Copy current match strings into the `kill-ring'. Default copy first group."
  (interactive "p" reb-mode)
  (with-current-buffer reb-target-buffer
    (kill-new
     (mapconcat
      (lambda (a)
        (when (equal (overlay-get a 'priority) priority)
          (format "%s
" (buffer-substring-no-properties (overlay-start a) (overlay-end a)))))
      reb-overlays))))
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



(provide 'init-misc)
;;; init-misc.el ends here

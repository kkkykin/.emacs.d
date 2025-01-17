;;; init-misc.el --- Misc Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

(defvar-keymap zr-mpc-prefix-map
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
(keymap-set ctl-x-map "c" zr-mpc-prefix-map)

(defun zr-always-yes (&rest args)
  "ref: https://goykhman.ca/gene/blog/2024-06-09-always-yes-in-emacs-lisp.html"
  (cl-letf (((symbol-function 'yes-or-no-p) #'always)
            ((symbol-function 'y-or-n-p) #'always))
    (funcall-interactively (car args) (cdr args))))


;; theme

(defcustom zr-all-theme-list (cons 'default (custom-available-themes))
  "list of Custom themes available for loading."
  :group 'my
  :type '(repeat symbol))

(defcustom zr-light-theme-list
  (cl-intersection
   zr-all-theme-list
   '(;; adwaita default dichromacy leuven tango tsdh-light whiteboard
     modus-operandi-deuteranopia modus-operandi modus-operandi-tinted
     modus-operandi-tritanopia))
  "Built-in light themes."
  :group 'my
  :type '(repeat symbol))

(defcustom zr-dark-theme-list
  (cl-intersection
   zr-all-theme-list
   '( deeper-blue leuven-dark manoj-dark misterioso
      modus-vivendi-deuteranopia modus-vivendi
      modus-vivendi-tinted modus-vivendi-tritanopia
      tango-dark tsdh-dark wheatgrass wombat))
  "Built-in dark themes."
  :group 'my
  :type '(repeat symbol))

;; (defcustom zr-fonts-list
;;   '(("霞鹜文楷等宽" . #1=(198 108 140 "https://github.com/lxgw/LxgwWenKai/releases"))
;;     ("LXGW WenKai Mono" . #1#)
;;     ("等距更纱黑体 SC" . #2=(188 108 130 "https://github.com/be5invis/Sarasa-Gothic/releases"))
;;     ("Sarasa Mono SC" . #2#)
;;     ("Unifont-JP" . (198 108 142 "https://unifoundry.com/unifont/index.html"))
;;     ("UnifontExMono" . (198 108 142 "https://github.com/stgiga/UnifontEX/releases")))
;;   "Fonts. Heights. Source."
;;   :group 'my
;;   :type '(repeat string))

;; (defun zr-setup-faces ()
;;   "Randomize setup faces."
;;   (when (display-graphic-p)
;;     (when-let* ((fonts
;;                 (cl-intersection
;;                  (font-family-list)
;;                  (mapcar #'car zr-fonts-list) :test 'equal)))
;;       (let ((font (assoc (seq-random-elt fonts) zr-fonts-list)))
;;         (set-face-attribute 'default nil :font (car font) :height
;;                             (cond ((< (display-pixel-width) 1920) (cadr font))
;;                                   ((> (display-pixel-width) 1920) (cadddr font))
;;                                   (t (caddr font))))))
;;     (zr-shuffle-set-theme (if (zr-system-dark-mode-enabled-p)
;;                               zr-dark-theme-list
;;                             zr-light-theme-list))))

(defcustom zr-fonts-list
  '(("LXGW WenKai Mono" (33 14 . 23) "https://github.com/lxgw/LxgwWenKai/releases")
    ("Sarasa Mono SC" (32 14 . 22) "https://github.com/be5invis/Sarasa-Gothic/releases")
    ("Maple Mono NF CN" (32 14 . 21) "https://github.com/subframe7536/maple-font/releases")
    ("Unifont-JP" #1=(33 14 . 24) "https://unifoundry.com/unifont/index.html")
    ("UnifontExMono" #1# "https://github.com/stgiga/UnifontEX/releases"))
  "Fonts. Heights. Source."
  :group 'my
  :type '(repeat string))

(defun zr-system-dark-mode-enabled-p ()
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

(defun zr-set-theme (theme)
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

(defun zr-shuffle-set-theme (&optional themes)
  "Shuffle set theme."
  (interactive "P")
  (let* ((themes (pcase themes
                   ('nil (if (zr-system-dark-mode-enabled-p)
                             zr-dark-theme-list
                           zr-light-theme-list))
                   ((guard (lambda (a) (consp arg)))
                    (if (zr-system-dark-mode-enabled-p)
                        zr-light-theme-list
                      zr-dark-theme-list))
                   (_ themes)))
         (theme (seq-random-elt themes)))
    (zr-set-theme theme)
    (message "Current theme: %s" (symbol-name theme))))

(defun zr-setup-faces ()
  "Randomize setup faces."
  (when (display-graphic-p)
    (when-let*
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
                        zr-fonts-list))))
      (set-face-attribute 'default nil :font (seq-random-elt fonts)))
    (zr-shuffle-set-theme)))
(add-hook 'window-setup-hook #'zr-setup-faces)

(defun zr-set-font-current-buffer (&optional font)
  "Set font for current buffer."
  (interactive "P")
  (let ((fonts (pcase font
                 ((pred stringp) (list font))
                 ('nil '("Unifont-JP" "UnifontExMono"))
                 ((pred listp) (list (completing-read
                                      "Fonts: " (font-family-list)))))))
    (when-let* ((font (cl-intersection
                      (font-family-list)
                      fonts :test 'equal)))
      (face-remap-add-relative 'default :family (car font)))))


;; android

(defcustom zr-android-misc-files-directory
  (locate-user-emacs-file "modules/android/")
  "Directory to store miscellaneous Android-related files."
  :group 'my
  :type 'directory)

(defcustom zr-emacs-keystore-file
  (expand-file-name "emacs-keystore" zr-android-misc-files-directory)
  "File path to save the emacs.keystore file."
  :group 'my
  :type 'file)

(defcustom zr-emacs-keystore-url
  "https://git.savannah.gnu.org/cgit/emacs.git/plain/java/emacs.keystore"
  "URL of the emacs.keystore file in the Emacs Git repository."
  :type 'string
  :group 'my)

(defun zr-download-emacs-keystore ()
  "Download the emacs.keystore file from the Emacs Git repository asynchronously.
The file will be saved to the `zr-android-misc-files-directory' directory."
  (interactive)
  (unless (file-directory-p zr-android-misc-files-directory)
    (make-directory zr-android-misc-files-directory t))
  (let* ((curl-command (list "curl" "-f" "-o"
                             zr-emacs-keystore-file zr-emacs-keystore-url))
         (curl-process (apply #'start-process "downloading-emacs-ks" nil curl-command)))
    (set-process-sentinel
     curl-process
     (lambda (process event)
       (pcase event
         ("finished\n"
          (message "emacs.keystore downloaded to %s" zr-emacs-keystore-file))
         (_ (error "Failed to download emacs.keystore")))))))

(defcustom zr-termux-root-directory "/data/data/com.termux/files/"
  "Andriod termux root path."
  :group 'my
  :type 'directory)

(defcustom zr-termux-tmp-directory (file-name-concat zr-termux-root-directory "home/tmp/")
  "Android termux tmp path."
  :group 'my
  :type 'directory)

(with-eval-after-load 'tramp
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux") "remote-shell"
                     (file-name-concat zr-termux-root-directory "usr/bin/bash")))
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "termux") "tmpdir" zr-termux-tmp-directory))
  (connection-local-set-profile-variables
   'tramp-connection-local-termux-profile
   `((tramp-remote-path
      . ,(mapcar
          (lambda (x)
            (if (stringp x) (concat zr-termux-root-directory x) x))
          (copy-tree tramp-remote-path)))
     (explicit-shell-file-name
      . ,(file-name-concat zr-termux-root-directory "usr/bin/bash"))))
  (connection-local-set-profiles
   ;; FIXME: If the username is not explicitly specified when accessing
   ;; a remote host, the :user option does not work. Therefore, remember
   ;; to use the 'sshx' method when accessing Termux, and 'scpx' otherwise.
   '(:application tramp :protocol "sshx" :user "t")
   'tramp-connection-local-termux-profile))


;; bookmark

(defcustom zr-bookmark-shared-prefix "s/"
  "Prefix of shared bookmark name."
  :group 'my
  :type 'string)

(defcustom zr-bookmark-shared-file (expand-file-name "bookmark-share" user-emacs-directory)
  "Shared bookmark file cross device."
  :group 'my
  :type 'file)
(when (file-exists-p zr-bookmark-shared-file)
  (bookmark-load zr-bookmark-shared-file nil t))

(defun zr-advice-bookmark-save (orig-fun &rest args)
  "Do not save shared bookmarks to local bookmark file."
  (with-temp-buffer
    (insert-file-contents zr-bookmark-shared-file)
    (let ((ori-shared (bookmark-alist-from-buffer))
          (new-local (copy-sequence bookmark-alist))
          new-shared)
      (dolist (bm new-local)
        (when (string-prefix-p zr-bookmark-shared-prefix (car bm))
          (setq new-local (delq bm new-local))
          (setq new-shared (cons bm new-shared))))
      (when-let* ((sorted (seq-sort-by #'car #'string< new-shared))
                 (need-update-p (not (equal sorted ori-shared)))
                 (bookmark-alist sorted))
        (funcall orig-fun nil zr-bookmark-shared-file nil))
      (let ((bookmark-alist new-local))
        (apply orig-fun args)))))
(advice-add 'bookmark-save :around 'zr-advice-bookmark-save)

(defun zr-advice-silence-messages (orig-fun &rest args)
  "Advice function that silences all messages in ORIG-FUN.
https://scripter.co/using-emacs-advice-to-silence-messages-from-functions"
  (let ((inhibit-message t)    ;Don't show the messages in Echo area
        (message-log-max nil)) ;Don't show the messages in the *Messages* buffer
    (apply orig-fun args)))


;; adb

(defun zr-adb-am (action activity)
  "ADB am command."
  (start-process "adb-am" nil "adb" "shell" "am" action activity))

(defun zr-am-start-activity (name)
  "Start activity through adb."
  (let ((activity (cond ((string= name "termux")
                         "com.termux/com.termux.HomeActivity"))))
    (zr-adb-am "start-activity" activity)))

(defun zr-am-force-stop (name)
  "Force-stop app through adb."
  (let ((package (cond ((string= name "termux") "com.termux"))))
    (zr-adb-am "force-stop" package)))


;; dired

(defun zr-dired-duplicate-file (arg)
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

(defun zr-dired-goto-random-file ()
  "Goto random file in current-buffer."
  (interactive nil dired-mode)
  (dired-goto-file
   (seq-random-elt
    (mapcan (lambda (a) (directory-files (car a) t "^[^.]" t)) dired-subdir-alist))))

(defun zr-advice-image-dired-create-thumb-maybe-gs (oldfun &rest args)
  (if (string= (file-name-extension (car args)) "pdf")
      (let ((image-dired-cmd-create-thumbnail-program "gs")
            (image-dired-cmd-create-thumbnail-options
             '("-sDEVICE=jpeg" "-dSAFER" "-r20" "-o" "%t" "%f")))
        (apply oldfun args))
    (apply oldfun args)))

(defun zr-dired-open-with-pandoc (&optional from to)
  "Open the current file in Dired using pandoc and display the result in Org mode.
Optional arguments:
  FROM: Input format for pandoc (default: auto-detected).
  TO: Output format for pandoc (default: org).
ref: https://pandoc.org/MANUAL.html#general-options"
  (interactive nil dired-mode)
  (let* ((file (dired-get-file-for-visit))
         (file-base (file-name-base file))
         (tformat (or to "org"))
         (buffer-name (generate-new-buffer-name
                       (file-name-with-extension
                        (concat file-base "--pandoc") tformat))))
    (with-current-buffer (get-buffer-create buffer-name)
      (apply #'call-process "pandoc" nil t nil file "-o-"
             (cl-delete-if
              #'null
              `(,(when from (concat "--from=" from))
                "-t" ,tformat)))
      (setq buffer-file-name
            (expand-file-name buffer-name (file-name-directory file)))
      (set-auto-mode)
      (read-only-mode)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (select-window (display-buffer (current-buffer))))))

(defun zr-shell-do-open (&optional arg)
  "Open file's directory using an external program. If called with
`universal-argument', then open file."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (shell-command-do-open (list (if arg file (file-name-directory file))))))

(with-eval-after-load 'dired
  (bind-keys
   :map dired-mode-map
   ;; z f available
   :prefix "SPC"
   :prefix-map zr-dired-spc-prefix-map
   ("d" . zr-dired-duplicate-file)
   ("o" . zr-dired-open-with-pandoc)
   ("s" . zr-dired-goto-random-file)))

(with-eval-after-load 'dired-aux
  (when (fboundp #'shell-command-do-open)
    (bind-keys
     :map zr-global-prefix-map
     ("E" . zr-shell-do-open))))

(with-eval-after-load 'image-dired
  (unless (executable-find "gm")
    (setq image-dired-cmd-create-thumbnail-program "ffmpeg"
          image-dired-cmd-create-thumbnail-options '("-y" "-i" "%f"
                                                     "-map_metadata" "-1"
                                                     "-vf" "scale=%w:-1"
                                                     "-f" "mjpeg" "%t"))
    (advice-add 'image-dired-create-thumb-1 :around #'zr-advice-image-dired-create-thumb-maybe-gs)))


;; speedbar
(defun zr-speedbar-show-unknown-files ()
  "Temporary show unknown files."
  (interactive)
  (let ((speedbar-show-unknown-files t))
    (speedbar-refresh)))

(defun zr-speedbar-item-diff ()
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
  (bind-keys
    :map speedbar-file-key-map
    ("=" . zr-speedbar-item-diff)
    ("(" . zr-speedbar-show-unknown-files)))


;; re-builder

(defun zr-wildcards-to-regexp (wildcards)
  "Convert a list of wildcard patterns to a regexp string."
  (mapconcat 
   (lambda (pattern)
     (string-replace "*" ".*" (string-replace "." "\\." pattern)))
   wildcards
   "\\|"))

(defun zr-reb-copy-match (&optional group)
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
  (keymap-set reb-mode-map "C-c M-w" 'zr-reb-copy-match))


;; isearch

(with-eval-after-load 'isearch
  (transient-define-prefix zr-isearch-menu ()
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
  (keymap-set isearch-mode-map "C-h t" 'zr-isearch-menu))

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

(defcustom zr-tab-line-excluded-buffer-list
  `(,(rx (| "*Async-native-compile-log*"
            "*Pp Eval Output*")))
  "Buffer which never show in tab-line.")

(defun zr-tab-line-tabs-buffer-group-by-mode-exclude-some-buffer
    (&optional buffer)
  "Exclude some buffer and group by mode."
  (when-let* ((buf (or buffer (current-buffer)))
              (exclude-p (cl-find-if-not
                          (lambda (a) (buffer-match-p a buf))
                          zr-tab-line-excluded-buffer-list)))
    (if (boundp 'tab-line-tabs-buffer-group-by-mode)
        (funcall 'tab-line-tabs-buffer-group-by-mode buf)
      (let ((tab-line-tabs-buffer-group-function nil))
        (funcall 'tab-line-tabs-buffer-group-name buf)))))

(setq tab-line-tabs-buffer-group-function
      #'zr-tab-line-tabs-buffer-group-by-mode-exclude-some-buffer)


;; repeat

(defvar-keymap zr-structure-repeat-map
  :repeat (:enter ( treesit-beginning-of-defun beginning-of-defun
                    treesit-end-of-defun end-of-defun
                    indent-pp-sexp prog-indent-sexp
                    c-beginning-of-defun c-end-of-defun
                    c-awk-beginning-of-defun c-awk-end-of-defun
                    python-nav-backward-up-list backward-up-list
                    python-shell-send-defun eval-defun))
  "r" #'raise-sexp
  "i" (lambda ()
        (interactive)
        (setq repeat-map 'zr-structure-repeat-map)
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
        (setq repeat-map 'zr-structure-repeat-map)
        (insert-pair nil ?\' ?\'))
  "\"" (lambda ()
         (interactive)
         (setq repeat-map 'zr-structure-repeat-map)
         (insert-pair nil ?\" ?\"))
  "<" (lambda ()
        (interactive)
        (setq repeat-map 'zr-structure-repeat-map)
        (insert-pair nil ?\< ?\>))
  "[" (lambda ()
        (interactive)
        (setq repeat-map 'zr-structure-repeat-map)
        (insert-pair nil ?\[ ?\]))
  "{" (lambda ()
        (interactive)
        (setq repeat-map 'zr-structure-repeat-map)
        (insert-pair nil ?\{ ?\}))
  "/" #'undo
  "w" #'hs-show-all
  "z" #'hs-hide-all
  "c" #'hs-toggle-hiding
  "u" (lambda ()
        (interactive)
        (setq repeat-map 'zr-structure-repeat-map)
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
        (setq repeat-map 'zr-structure-repeat-map)
        (pcase major-mode
          ((guard (memq major-mode '(python-ts-mode)))
           (treesit-beginning-of-defun))
          ((guard (memq major-mode '(c++-mode c-mode)))
           (c-beginning-of-defun))
          ('awk-mode
           (c-awk-beginning-of-defun))
          (_ (beginning-of-defun))))
  "e" (lambda ()
        (interactive)
        (setq repeat-map 'zr-structure-repeat-map)
        (pcase major-mode
          ((guard (memq major-mode '(python-ts-mode)))
           (treesit-end-of-defun))
          ((guard (memq major-mode '(c++-mode c-mode)))
           (c-end-of-defun))
          ('awk-mode
           (c-awk-end-of-defun))
          (_ (end-of-defun))))
  "x" (lambda ()
        (interactive)
        (setq repeat-map 'zr-structure-repeat-map)
        (pcase major-mode
          ((guard (memq major-mode '(python-ts-mode)))
           (python-shell-send-defun))
          (_ (eval-defun)))))


;; file

(defun zr-file-modified-recently-p (file seconds)
  "Check file is modified recently."
  (and (file-exists-p file)
       (time-less-p
        (current-time)
        (time-add
         (file-attribute-modification-time
          (file-attributes file))
         seconds))))


;; etc

(defun zr-generate-uuid (&optional obj)
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
(defun zr-generate-pass (&optional arg)
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

(defun zr-pure-pure-pure-url (url)
  "Remove invalid char in url."
  (interactive (list (read-string "Url: " nil nil (current-kill 0 t))))
  (kill-new (replace-regexp-in-string
             (concat "[^" url-get-url-filename-chars "]+") "" url)))


;; appt

(defun zr-notifications-notify (title body &rest params)
  "Send system notification with TITLE and BODY based on current OS.
   On windows, an active notification must be removed by calling
   `w32-notification-close' before a new one can be shown."
  (when-let* ((notify-fn (pcase system-type
                           ('android #'android-notifications-notify)
                           ('windows-nt #'w32-notification-notify)
                           ('gnu/linux #'notifications-notify)))
              (id (apply notify-fn :title title :body body params))
              (timeout (plist-get params :timeout))
              ((eq system-type 'windows-nt)))
    (run-with-timer timeout nil (lambda () (w32-notification-close id)))))

(defun zr-appt-notification-notify (min-to-app new-time appt-msg)
  "Display appointment due in MIN-TO-APP (a string) minutes.
NEW-TIME is a string giving the current date.
Displays the appointment message APPT-MSG via notification.
ref: `appt-disp-window'"
  (let ((timeout (and (eq system-type 'windows-nt) 0)))
    (and (listp min-to-app)
         (setq min-to-app (number-to-string appt-display-interval)
               appt-msg (mapconcat #'identity appt-msg "\n")))
    (zr-notifications-notify
     (message "In %s minutes" min-to-app)
     appt-msg
     :urgency 'critical
     :replaces-id 100
     :timeout timeout)))

(defun zr-appt-habits ()
  "Add some habits to appointments."
  (dolist (h (number-sequence 8 23))
    (appt-add (format "%d:00" h) "💧 Stay hydrated!" 0)))

(with-eval-after-load 'appt
  (setq appt-disp-window-function #'zr-appt-notification-notify)
  (zr-appt-habits))

(with-eval-after-load 'midnight
  (add-hook 'midnight-hook #'zr-appt-habits))


;; proc

(defun zr-get-pid-from-file (pid-file)
  "Read and return the process ID (PID) from the given PID file.
If the file doesn't exist or is not readable, return nil.
If the file is empty or doesn't contain a valid integer, return nil.

PID-FILE is the path to the file containing the process ID."
  (when (file-readable-p pid-file)
    (with-temp-buffer
      (insert-file-contents-literally pid-file)
      (let ((pid-string (buffer-string)))
        (unless (string-empty-p pid-string)
          (let ((pid (string-to-number pid-string)))
            (when (and (integerp pid) (> pid 0))
              pid)))))))

(defun zr-proc-menu-do-kill-line (cnt)
  "Delete CNT entries from the process menu without killing their processes.
Removes the entries from `tabulated-list-entries' while preserving the
actual processes. CNT is the prefix argument indicating how many entries
to delete."
  (interactive "p")
  (let (deleted)
    (dotimes (i cnt)
      (push (tabulated-list-delete-entry) deleted))
    (setq tabulated-list-entries
          (cl-nset-difference tabulated-list-entries deleted :key #'car))))

(defun zr-proc-menu-do-delete-process (cnt)
  "Kill CNT processes from the process menu and remove their entries.
If point is on a valid entry, kills CNT processes starting from current
position.  Otherwise, kills all remaining processes from the next entry
onwards.  CNT is the prefix argument specifying number of processes to
kill.  Refreshes the buffer after deletion."
  (interactive "p")
  (let ((revert-buffer-function #'ignore))
    (if (tabulated-list-get-id)
        (dotimes (i cnt)
          (process-menu-delete-process)
          (forward-line))
      (forward-line)
      (while (tabulated-list-get-id)
        (process-menu-delete-process)
        (forward-line))))
  (revert-buffer))

(defvar-local zr-proc-menu-group-by-index nil
  "Column key used for grouping process entries in the tabulated list.
When nil, no grouping is performed. Otherwise, should be a valid index
into `tabulated-list-format' array.")

(defun zr-proc-menu-group-by ()
  "Group process menu entries based on `zr-proc-menu-group-by-index'.
When `zr-proc-menu-group-by-index' is set, groups entries in
`tabulated-list-entries' by the specified column value, prefixing group
headers with '* '."
  (when zr-proc-menu-group-by-index
    (setq tabulated-list-groups
          (seq-group-by (lambda (entry)
                          (let ((s (aref (cadr entry) zr-proc-menu-group-by-index)))
                            (concat "* " (if (stringp s) s (car s)))))
                        tabulated-list-entries))))

(defun zr-proc-menu-do-group (&optional index)
  "Set grouping for process menu based on column specified by INDEX.
INDEX is the prefix argument indicating which column to group
by (1-based index).  When INDEX is 0 or nil, grouping is disabled.
Refreshes the buffer after changing grouping."
  (interactive "p")
  (setq zr-proc-menu-group-by-index
        (if (zerop index) nil
          (1- (min index (length tabulated-list-format)))))
  (zr-proc-menu-group-by)
  (revert-buffer))

(defun zr-proc-menu-setup ()
  "Set up process menu functionality."
  (add-hook 'tabulated-list-revert-hook #'zr-proc-menu-group-by 50 t))

(add-hook 'process-menu-mode-hook #'zr-proc-menu-setup)

(bind-keys
 :map process-menu-mode-map
 ("k" . zr-proc-menu-do-kill-line)
 ("/" . zr-proc-menu-do-group)
 ("d" . zr-proc-menu-do-delete-process))


;; follow
(defun zr-follow-current-window (&optional arg)
  "Follow the window."
  (interactive "P")
  (let* ((window-width (window-text-width))
         (split-cnt (1- (/ window-width fill-column)))
         (single-width (/ window-width (1+ split-cnt))))
    (pcase arg
      ('(4) (delete-other-windows))
      ((and `(,state . ,keymap)
            (guard (memq state '(vi-state insert-state emacs-state))))
       (viper-add-local-keys state keymap)))
    (dotimes (i split-cnt)
      (split-window nil (* (- split-cnt i) single-width) t))
    (follow-mode 1)))

(with-eval-after-load 'viper
  (bind-keys
   :map zr-global-prefix-map
   ("F" . zr-follow-current-window)))

(provide 'init-misc)
;;; init-misc.el ends here

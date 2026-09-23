;;; init-misc.el --- Misc Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'multisession)
(require 'transient)
(require 'tab-line)
(require 'bookmark)

(declare-function shell-command-do-open "dired-aux")
(declare-function dired-goto-file "dired")
(declare-function dired-revert "dired")
(declare-function dired-get-file-for-visit "dired")
(declare-function dbus-call-method "dbus")
(declare-function dbus-ignore-errors "dbus")
(declare-function dframe-close-frame "dframe.el")
(declare-function speedbar-line-file "speedbar")
(declare-function speedbar-refresh "speedbar")
(declare-function xterm--query "xterm")
(declare-function zr-rclone-normalize-remote-path "init-rclone")
(declare-function zr-rclone-directory-files-recursively "init-rclone")
(declare-function zr-rclone-list-remotes "init-rclone")
(declare-function org-id-new "org-id")
(declare-function zr-android-termux-notifications-notify "init-android")
(declare-function notifications-notify "notifications")
(declare-function android-notifications-notify "androidselect.c")
(declare-function viper-add-local-keys "viper-keym")
(declare-function zr-rclone-copy-file "init-rclone")
(declare-function zr-rclone-url-retrieve "init-rclone")
(declare-function wallpaper--image-file-regexp "wallpaper")
(declare-function term-send-string "term")

(defun zr-always-yes (&rest args)
  "ref: https://goykhman.ca/gene/blog/2024-06-09-always-yes-in-emacs-lisp.html"
  (cl-letf (((symbol-function 'yes-or-no-p) #'always)
            ((symbol-function 'y-or-n-p) #'always))
    (funcall-interactively (car args) (cdr args))))

(defgroup zr nil
  "Used by this configuration."
  :group 'environment
  :prefix "zr-")

(defcustom zr-dotfiles-dir-followd-by-vars nil
  "List of variables that should be re-evaluated when `zr-dotfiles-dir' changes.
This allows dependent settings to update automatically when the
directory changes."
  :type '(repeat variable)
  :group 'zr)

(defcustom zr-dotfiles-dir (expand-file-name "~/.config")
  "The directory where dotfiles are stored.
This is typically set to a directory like `~/.config' but can be
customized.  When this value is changed, any variables listed in
`zr-dotfiles-dir-followd-by-vars' are re-evaluated to ensure dependent
settings are updated."
  :type 'directory
  :group 'zr
  :set (lambda (sym val)
         (set-default sym val)
         (dolist (var zr-dotfiles-dir-followd-by-vars)
           (when (boundp var)
             (custom-reevaluate-setting var)))))

(defcustom zr-secrets-dir-followd-by-vars nil
  "List of variables that should be re-evaluated when `zr-secrets-dir' changes.
This allows dependent settings to update automatically when the
directory changes."
  :type '(repeat variable)
  :group 'zr)

(defcustom zr-secrets-dir (expand-file-name "~/secrets")
  "Secrets."
  :type 'directory
  :group 'zr
  :set (lambda (sym val)
         (set-default sym val)
         (when (file-directory-p val)
           (chmod val #o700))
         (dolist (var zr-secrets-dir-followd-by-vars)
           (when (boundp var)
             (custom-reevaluate-setting var)))))


;; android

(defcustom zr-android-misc-files-directory
  (locate-user-emacs-file "modules/android/")
  "Directory to store miscellaneous Android-related files."
  :group 'zr
  :type 'directory)

(defcustom zr-emacs-keystore-file
  (expand-file-name "emacs-keystore" zr-android-misc-files-directory)
  "File path to save the emacs.keystore file."
  :group 'zr
  :type 'file)

(defcustom zr-emacs-keystore-url
  "https://git.savannah.gnu.org/cgit/emacs.git/plain/java/emacs.keystore"
  "URL of the emacs.keystore file in the Emacs Git repository."
  :type 'string
  :group 'zr)

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
     (lambda (_ event)
       (pcase event
         ("finished\n"
          (message "emacs.keystore downloaded to %s" zr-emacs-keystore-file))
         (_ (error "Failed to download emacs.keystore")))))))

(defcustom zr-termux-root-directory "/data/data/com.termux/files/"
  "Andriod termux root path."
  :group 'zr
  :type 'directory)

(defcustom zr-termux-tmp-directory (file-name-concat zr-termux-root-directory "usr/tmp/")
  "Android termux tmp path."
  :group 'zr
  :type 'directory)

(defvar tramp-connection-properties)
(defvar tramp-remote-path)
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
  :group 'zr
  :type 'string)

(defcustom zr-bookmark-shared-file (expand-file-name "bookmark-share" user-emacs-directory)
  "Shared bookmark file cross device."
  :group 'zr
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

(defvar dired-subdir-alist)
(defun zr-dired-goto-random-file ()
  "Goto random file in current-buffer."
  (interactive nil dired-mode)
  (dired-goto-file
   (seq-random-elt
    (mapcan (lambda (a) (directory-files (car a) t "^[^.]" t)) dired-subdir-alist))))

(defvar image-dired-cmd-create-thumbnail-program)
(defvar image-dired-cmd-create-thumbnail-options)
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

(defvar dired-mode-map)
(with-eval-after-load 'dired
  (bind-keys
   :map dired-mode-map
   ;; z f available
   :prefix "SPC"
   :prefix-map zr-dired-spc-prefix-map
   ("d" . zr-dired-duplicate-file)
   ("o" . zr-dired-open-with-pandoc)
   ("s" . zr-dired-goto-random-file)))

(defvar zr-menu)
(with-eval-after-load 'dired-aux
  (when (fboundp #'shell-command-do-open)
    (define-key zr-menu [zr-shell-do-open]
                '(menu-item "zr-shell-do-open" zr-shell-do-open))))

(with-eval-after-load 'image-dired
  (unless (executable-find "gm")
    (setq image-dired-cmd-create-thumbnail-program "ffmpeg"
          image-dired-cmd-create-thumbnail-options '("-y" "-i" "%f"
                                                     "-map_metadata" "-1"
                                                     "-vf" "scale=%w:-1"
                                                     "-f" "mjpeg" "%t"))
    (advice-add 'image-dired-create-thumb-1 :around #'zr-advice-image-dired-create-thumb-maybe-gs)))


;; speedbar

(defvar speedbar-show-unknown-files)
(defun zr-speedbar-show-unknown-files ()
  "Temporary show unknown files."
  (interactive)
  (let ((speedbar-show-unknown-files t))
    (speedbar-refresh)))

(defvar speedbar-last-selected-file)
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

(defvar speedbar-file-key-map)
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

(defvar reb-target-buffer)
(defvar reb-overlays)
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

(defvar reb-mode-map)
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
  "Buffer which never show in tab-line."
  :type 'regexp
  :group 'zr)

(defun zr-tab-line-tabs-buffer-group-by-mode-exclude-some-buffer
    (&optional buffer)
  "Exclude some buffers and group the others BUFFER by mode."
  (when-let* ((buf (or buffer (current-buffer)))
              ((not (cl-find-if
                     (lambda (regexp)
                       (buffer-match-p regexp buf))
                     zr-tab-line-excluded-buffer-list))))
    (if (fboundp 'tab-line-tabs-buffer-group-by-mode)
        (tab-line-tabs-buffer-group-by-mode buf)
      (buffer-local-value 'major-mode buf))))

(setq tab-line-tabs-buffer-group-function
      #'zr-tab-line-tabs-buffer-group-by-mode-exclude-some-buffer)


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

(declare-function zr-rclone-normalize-remote-path "init-rclone")
(declare-function zr-rclone-directory-files-recursively "init-rclone")
(declare-function zr-rclone-list-remotes "init-rclone")

(defun zr-directory-files-recursively
    (dir regexp &optional include-directorys)
  "Like `directory-files-recursively', but works on rclone."
  (if (file-exists-p dir)
      (directory-files-recursively dir regexp include-directorys)
    (require 'init-rclone)
    (pcase-let ((`(,fs ,remote)
                 (string-split dir ":")))
      (when (member fs (zr-rclone-list-remotes))
        (zr-rclone-directory-files-recursively
         fs (zr-rclone-normalize-remote-path remote)
         regexp include-directorys)))))


;; etc

(defvar org-id-method)
(declare-function org-id-new "org-id")

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
        (uv-program "uv")
        (xc-program "keepassxc-cli")
        (gpg-program "gpg")
        (ssl-program "openssl")
        (alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (setq pass
          (cond
           ((executable-find uv-program)
            (car (process-lines uv-program "run" "python" "-c" "import secrets,string; print(''.join(secrets.choice(string.ascii_letters + string.digits + string.punctuation) for _ in range(15)))")))
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

(defvar url-get-url-filename-chars)
(defun zr-pure-pure-pure-url (url)
  "Remove invalid char in url."
  (interactive (list (read-string "Url: " nil nil (current-kill 0 t))))
  (kill-new (replace-regexp-in-string
             (concat "[^" url-get-url-filename-chars "]+") "" url)))


;; appt
(declare-function zr-android-termux-notifications-notify "init-android")
(declare-function notifications-notify "notifications")
(declare-function android-notifications-notify "androidselect.c")

(defun zr-notifications-notify (title body &rest params)
  "Send system notification with TITLE and BODY based on current OS.
   On windows, an active notification must be removed by calling
   `w32-notification-close' before a new one can be shown."
  (when-let* ((notify-fn (pcase system-type
                           ('android (if (boundp #'android-notifications-notify)
                                         #'android-notifications-notify
                                       #'zr-android-termux-notifications-notify))
                           ('windows-nt #'w32-notification-notify)
                           ((guard (fboundp #'notifications-notify))
                            #'notifications-notify)))
              (id (apply notify-fn :title title :body body params))
              (timeout (plist-get params :timeout))
              ((eq system-type 'windows-nt)))
    (run-with-timer timeout nil (lambda () (w32-notification-close id)))))

(defvar appt-display-interval)
(defun zr-appt-notification-notify (min-to-app _ appt-msg)
  "Display appointment due in MIN-TO-APP (a string) minutes.
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

(defvar appt-disp-window-function)
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
    (dotimes (_ cnt)
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
        (dotimes (_ cnt)
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
  (setq tabulated-list-groups
        (when zr-proc-menu-group-by-index
          (seq-group-by (lambda (entry)
                          (let ((s (aref (cadr entry)
                                         zr-proc-menu-group-by-index)))
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

(defvar-local zr-proc-menu-omit-regexp
    (rx bos
        (| (: "server" (? " <" (+ (| ?. ?: digit)) ?>))
           "ispell")
        eos)
  "Regular expression to match process names that should be omitted from
the process menu.")

(defun zr-proc-menu-omit-proc (&optional regexp)
  "Filter out processes from the process menu that match
`zr-proc-menu-omit-regexp'. When called interactively, set
`zr-proc-menu-omit-regexp' from input."
  (interactive (list (read-regexp "Omit-regexp: ")) process-menu-mode)
  (when regexp
    (setq zr-proc-menu-omit-regexp regexp))
  (setq tabulated-list-entries
        (cl-delete-if (lambda (p) (string-match-p
                              zr-proc-menu-omit-regexp
                              (aref (cadr p) 0)))
                      tabulated-list-entries)))

(define-minor-mode zr-proc-menu-omit-mode
  "Omit processes from the process menu based on `zr-proc-menu-omit-regexp'.
When enabled, processes matching the regular expression are omitted from
the process menu."
  :init-value nil
  :lighter " O"
  (if zr-proc-menu-omit-mode
      (add-hook 'tabulated-list-revert-hook #'zr-proc-menu-omit-proc 25 t)
    (remove-hook 'tabulated-list-revert-hook #'zr-proc-menu-omit-proc t)))

(defun zr-proc-menu-setup ()
  "Set up process menu functionality."
  (zr-proc-menu-omit-mode t)
  (add-hook 'tabulated-list-revert-hook #'zr-proc-menu-group-by 50 t))

(add-hook 'process-menu-mode-hook #'zr-proc-menu-setup)

(bind-keys
 :map process-menu-mode-map
 ("(" . zr-proc-menu-omit-mode)
 (")" . zr-proc-menu-omit-proc)
 ("k" . zr-proc-menu-do-kill-line)
 ("/" . zr-proc-menu-do-group)
 ("d" . zr-proc-menu-do-delete-process))


;; follow
(declare-function viper-add-local-keys "viper-keym")

(defun zr-follow-current-window (&optional arg)
  "Follow the window."
  (interactive "P")
  (pcase arg
    ('(4) (delete-other-windows))
    ((and `(,state . ,keymap)
          (guard (memq state '(vi-state insert-state emacs-state))))
     (viper-add-local-keys state keymap)))
  (let* ((window-width (window-text-width))
         (split-cnt (1- (/ window-width fill-column)))
         (single-width (/ window-width (1+ split-cnt))))
    (dotimes (i split-cnt)
      (split-window nil (* (- split-cnt i) single-width) t))
    (follow-mode 1)))

(with-eval-after-load 'viper
  (define-key zr-menu [zr-follow-current-window]
              '(menu-item "zr-follow-current-window" zr-follow-current-window)))


;; wallpaper

(defcustom zr-wallpaper-directory nil
  "Directories containing wallpaper images.
Can be a single directory or a list of directories."
  :type (or 'directory
            (repeat 'directory)))

(defvar zr-rclone-rc-function)
(declare-function zr-rclone-copy-file "init-rclone")
(declare-function zr-rclone-url-retrieve "init-rclone")
(declare-function wallpaper--image-file-regexp "wallpaper")

(defun zr-set-wallpaper-randomly ()
  "Set a random wallpaper image from `zr-wallpaper-directory'."
  (interactive)
  (require 'wallpaper)
  (let ((image-regexp (wallpaper--image-file-regexp))
        imgs)
    (dolist (dir (ensure-list zr-wallpaper-directory))
      (setq imgs (append (zr-directory-files-recursively dir image-regexp) imgs)))
    (let ((img (seq-random-elt imgs)))
      (if (file-exists-p img)
          (wallpaper-set img)
        (require 'init-rclone)
        (pcase-let*
            ((`(,fs . ,remote)
              (string-split img ":"))
             (prefix "emacs-wallpaper-")
             (base (make-temp-name prefix))
             (abs (expand-file-name base temporary-file-directory))
             (zr-rclone-rc-function #'zr-rclone-url-retrieve))
          (when-let* ((trash (directory-files
                              temporary-file-directory t
                              (concat "^" prefix) t)))
            (mapc #'delete-file trash))
          (zr-rclone-copy-file fs (string-join remote ":")
                               temporary-file-directory base)
          (wallpaper-set abs))))
    (unless imgs
      (user-error "No image files found in `zr-wallpaper-directory'"))))

;; term

(defvar explicit-shell-file-name)
(declare-function term-send-string "term")

(defun zr-gpg-term ()
  "Start term and automatically execute commands."
  (interactive)
  (ansi-term (or explicit-shell-file-name shell-file-name))
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (term-send-string proc "set +o history\n")
    (dolist (cmd '("SSH_AUTH_SOCK=\"$(gpgconf --list-dirs agent-ssh-socket)\""
                   "export SSH_AUTH_SOCK"
                   "GPG_TTY=\"$(tty)\""
                   "export GPG_TTY"
                   "gpg-connect-agent updatestartuptty /bye > /dev/null"))
      (term-send-string proc (concat cmd "\n")))
    (term-send-string proc "set -o history\n")))

;; Moyu

(defvar zr-moyu-buffers nil
  "All moyu buffers.")

(define-minor-mode zr-moyu-mode
  "Minor mode for \"Moyu\" (slacking off).
When active, attempts to disable the Input Method Editor (IME)
on Windows systems to prevent accidental typing. It also tries
to keep the IME disabled when Emacs frame focus changes or when
switching buffers.
This mode currently only affects Windows systems with IME support."
  :init-value nil
  :lighter " mo"
  (if zr-moyu-mode
      (progn
        (zr-moyu-setup)
        (push (current-buffer) zr-moyu-buffers)
        (add-function :after after-focus-change-function #'zr-moyu-setup)
        (add-hook 'window-buffer-change-functions #'zr-moyu-setup nil t)
        (add-hook 'window-selection-change-functions #'zr-moyu-setup nil t))
    (zr-moyu-remove-buffer (current-buffer))
    (remove-hook 'window-buffer-change-functions #'zr-moyu-setup t)
    (remove-hook 'window-selection-change-functions #'zr-moyu-setup t)))

(defun zr-moyu-remove-buffer (&optional buf)
  (setq zr-moyu-buffers
        (delete (or buf (current-buffer)) zr-moyu-buffers))
  (when (seq-empty-p zr-moyu-buffers)
    (remove-function after-focus-change-function #'zr-moyu-setup)))
(add-hook 'kill-buffer-hook #'zr-moyu-remove-buffer)

(defun zr-moyu-quit-window ()
  "Quit all `zr-moyu-mode' buffers."
  (interactive)
  (mapc #'quit-windows-on zr-moyu-buffers))

(defun zr-set-ime-open-status (status)
  "Set the IME open status to STATUS on Windows systems.
Does nothing on other operating systems. STATUS should be non-nil
to enable IME, nil to disable."
  (pcase system-type
    ('windows-nt (w32-set-ime-open-status status))))

(defun zr-moyu-setup (&optional _frame)
  "Disable IME when focus changes if `zr-moyu-mode' is active.
Intended for use with `after-focus-change-function'.
FRAME argument is ignored."
  (when (and zr-moyu-mode
             (frame-focus-state))
    (zr-set-ime-open-status nil)))

;; json

(defun zr-parse-json-file (file)
  "Parse JSON from FILE and return its Lisp representation."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer)))

(defun zr-merge-json-files (file1 file2 output-file)
  "Merge JSON FILE1 and FILE2 into OUTPUT-FILE.
Values from FILE2 take priority over values from FILE1.
JSON objects are merged recursively, while arrays are concatenated."
  (let* ((json1 (zr-parse-json-file file1))
         (json2 (zr-parse-json-file file2))
         (merged (zr-merge-json-objects json1 json2)))
    (with-temp-file output-file
      (json-insert merged))))

(defun zr-merge-json-objects (obj1 obj2)
  "Recursively merge JSON values OBJ1 and OBJ2.
Values from OBJ2 take priority over values from OBJ1.
JSON objects are merged recursively and arrays are concatenated."
  (cond
   ;; JSON objects.
   ((and (hash-table-p obj1)
         (hash-table-p obj2))
    (zr-merge-json-hash-tables obj1 obj2))

   ;; JSON arrays.
   ((and (vectorp obj1)
         (vectorp obj2))
    (vconcat obj1 obj2))

   ;; Primitive values and mismatched types.
   (t obj2)))

(defun zr-merge-json-hash-tables (table1 table2)
  "Merge JSON object TABLE1 and TABLE2.
Values from TABLE2 take priority."
  (let ((result (copy-hash-table table1)))
    (maphash
     (lambda (key value)
       (if (gethash key result)
           (puthash key
                    (zr-merge-json-objects (gethash key result) value)
                    result)
         (puthash key value result)))
     table2)
    result))

(defun zr-sops-file-contents (file)
  "Return the decrypted contents of FILE using sops."
  (when (file-exists-p file)
    (with-temp-buffer
      (call-process "sops" nil t nil "-d" file)
      (buffer-string))))

(defun zr-sops-parse-json (file)
  "Parse and return the decrypted contents of FILE as JSON."
  (when-let* ((s (zr-sops-file-contents file)))
    (json-parse-string s)))

;; debug
(defun zr-insert-declarations-from-compile-log (warning-buffer)
  "Insert declarations for warnings in WARNING-BUFFER.

Function warnings generate `declare-function' forms.
Free-variable warnings generate `defvar' forms.

Target buffers are found by the file names in the warnings."
  (interactive
   (list (read-buffer "Warning buffer: " "*Compile-Log*" t)))
  (let ((warnings (make-hash-table :test #'equal)))
    ;; Collect warnings, grouped by file.
    (with-current-buffer warning-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward
                "^\\([^:\n]+\\.el\\):[0-9]+:[0-9]+: Warning: \\(.*\\)$"
                nil t)
          (push (match-string-no-properties 2)
                (gethash (match-string-no-properties 1) warnings)))))

    (let ((inserted 0)
          (not-found nil))
      (maphash
       (lambda (file warning-list)
         (let ((buffer
                (seq-find
                 (lambda (buffer)
                   (with-current-buffer buffer
                     (and buffer-file-name
                          (string-equal
                           file
                           (file-name-nondirectory buffer-file-name)))))
                 (buffer-list)))
               functions
               variables)
           (if (not buffer)
               (push file not-found)
             ;; Parse warnings.
             (dolist (warning warning-list)
               (cond
                ((string-match
                  "the function `\\([^']+\\)' is not known to be defined"
                  warning)
                 (push (intern (match-string 1 warning))
                       functions))
                ((or (string-match
                      "\\(?:reference\\|assignment\\) to free variable `\\([^']+\\)'"
                      warning)
                     (string-match
                      "Unused lexical variable `\\([^']+\\)'"
                      warning))
                 (push (intern (match-string 1 warning))
                       variables))))

             (setq functions (delete-dups functions)
                   variables (delete-dups variables))

             (with-current-buffer buffer
               (let (forms)
                 ;; declare-function
                 (dolist (function functions)
                   (unless
                       (save-excursion
                         (goto-char (point-min))
                         (re-search-forward
                          (format
                           "^(declare-function[ \t]+%s\\(?:[ \t\n)]\\)"
                           (regexp-quote (symbol-name function)))
                          nil t))
                     (let* ((source (symbol-file function 'defun))
                            (library (and source
                                          (file-name-base source)))
                            )
                       (push
                        (format "(declare-function %s %S)"
                                function
                                library)
                        forms))))

                 ;; defvar
                 (dolist (variable variables)
                   (unless
                       (save-excursion
                         (goto-char (point-min))
                         (re-search-forward
                          (format
                           "^(defvar[ \t]+%s\\(?:[ \t\n)]\\)"
                           (regexp-quote (symbol-name variable)))
                          nil t))
                     (push
                      (format "(defvar %s)" variable)
                      forms)))

                 (when forms
                   (goto-char (point-min))
                   (insert (string-join (nreverse forms) "\n")
                           "\n\n")
                   (setq inserted (+ inserted (length forms)))))))))
       warnings)

      (message "Inserted %d declarations%s"
               inserted
               (if not-found
                   (format "; buffers not found: %s"
                           (string-join not-found ", "))
                 "")))))

(provide 'init-misc)
;;; init-misc.el ends here

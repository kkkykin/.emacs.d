;;; init-rclone.el --- Rclone RC function            -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <kkky@KKSBOW>
;; Keywords: processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


;; rclone

(defcustom zr-rclone-baseurl "http://127.0.0.1:5572"
  "Default rclone rc baseurl.")
(with-eval-after-load 'init-net
  (add-to-list 'zr-net-url-auth-urls (regexp-quote zr-rclone-baseurl)))

(defcustom zr-rclone-root-directory (pcase system-type
                                      ('windows-nt "d:/rclone/")
                                      ('gnu/linux (expand-file-name
                                                   "~/rclone/")))
  "Default rclone root directory."
  :type 'directory)

(defcustom zr-rclone-rc-function #'zr-rclone-call-curl
  "Function to interact with rclone rc api.")

(defun zr-rclone-url-retrieve (user baseurl action args)
  "Make HTTP call to rclone RC API using url-retrieve-synchronously."
  (let ((url (concat baseurl "/" action))
        (url-request-method "POST")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Basic " (base64-encode-string user)))))
        (url-request-data args))
    (when args
      (push '("Content-Type" . "application/json") url-request-extra-headers))
    (url-insert-file-contents url)))

(defun zr-rclone-call-curl (user baseurl action args)
  "Make curl call to rclone RC API."
  (apply #'call-process "curl" nil (current-buffer) nil
         "-su" user "-XPOST"
         (concat baseurl "/" action)
         (when args
           (append '("-H" "Content-Type: application/json" "-d")
                   (list args)))))

(defun zr-rclone-rc-contact (action &optional args json-type async baseurl)
  "Contact with rclone."
  (with-current-buffer (get-buffer-create "*rclone-rc*")
    (when-let* ((begin (point-max))
                (url (url-generic-parse-url zr-rclone-baseurl))
                (auth (car
                       (auth-source-search :max 1
                                           :host (url-host url)
                                           :port (url-port url))))
                (user (concat (plist-get auth :user)
                              ":" (auth-info-password auth))))
      (goto-char begin)
      (funcall zr-rclone-rc-function
               user (or baseurl zr-rclone-baseurl) action args)
      (with-restriction begin (point-max)
        (goto-char begin)
        (apply #'json-parse-buffer json-type)))))

;; [[elisp:(zr-rclone-rc-contact "rc/noop" (json-encode '(("potato" . 1) ("sausage" . 2))))]]

;; [[elisp:(zr-rclone-rc-contact "config/listremotes")]]

(defun zr-rclone-list-remotes ()
  "Lists the remotes in the config file and defined in environment
variables."
  (gethash "remotes"
           (zr-rclone-rc-contact "config/listremotes" nil
                                 '(:array-type list))))

(defun zr-rclone-list-mounts ()
  "Show current mount points."
  (plist-get (zr-rclone-rc-contact "mount/listmounts" nil
                                   '(:object-type plist))
             :mountPoints))

(defun zr-rclone-mount-remote (remote &optional root remote-path)
  "Create a new mount point."
  (interactive
   (list (completing-read "Remote: "
                          (cl-delete-if
                           (lambda (r) (string-prefix-p "local" r))
                           (zr-rclone-list-remotes)))))
  (let* ((root (or root zr-rclone-root-directory))
         (path (expand-file-name remote root)))
    (make-directory root t)
    (zr-rclone-rc-contact
     "mount/mount"
     (json-encode `(("fs" . ,(concat remote ":" (or remote-path "")))
                    ("mountPoint" . ,path)
                    ("vfsOpt" . ,(json-encode '(("vfs-cache-mode" . "writes")))))))
    (when (called-interactively-p t)
      (dired path))))

(defun zr-rclone-unmount (point)
  "Unmount selected active mount."
  (interactive
   (list (completing-read "Point: "
                          (mapcar (lambda (m) (plist-get m :MountPoint))
                                  (zr-rclone-list-mounts)))))
  (zr-rclone-rc-contact
   "mount/unmount"
   (json-encode `(("mountPoint" . ,point)))))

(defun zr-rclone-unmount-all ()
  "Unmount all active mounts."
  (interactive)
  (zr-rclone-rc-contact "mount/unmountall"))

(defun zr-rclone-android-notification-handler (id event)
  (pcase event
    (_
     (android-notification-notify
      :title "Quitting emacs rclone"
      :body "Quitting emacs rclone"
      :replaces-id id
      :timeout 1000)
     (zr-rclone-quit))))

(defun zr-rclone-rc-start ()
  (interactive)
  (let* ((auth (car (zr-net-url-get-auths zr-rclone-baseurl)))
         (addr (format "--rc-addr=%s:%s" (plist-get auth :host) (plist-get auth :port)))
         (user (concat "--rc-user=" (plist-get auth :user)))
         (pass (concat "--rc-pass=" (auth-info-password auth)))
         (par (list addr user pass)))
    (when (eq system-type 'android)
      (android-nofitication-notify
       :title "Emacs Rclone"
       :body "Rclone RC Running"
       :actions '("quit" "quit")
       :resident t
       :on-action #'zr-rclone-android-notification-handler
       :on-close #'zr-rclone-android-notification-handler))
    (apply #'call-process "rclone" nil 0 nil "rcd" "--rc-serve" par)))

(defun zr-rclone-quit ()
  "Exit rclone safely."
  (interactive)
  (zr-rclone-rc-contact "core/quit"))

(defun zr-rclone-normalize-remote-path (remote-path)
  "Make REMOTE-PATH fit rclone's need."
  (replace-regexp-in-string
   (rx bos (? ?/) (group-n 1 (*? anychar)) (? ?/) eos)
   "\\1"
   remote-path))

(defun zr-rclone-directory-files-internal (fs remote &optional opt)
  "Returns a list of files in the remote directory specified by `remote`.
   The `fs` argument specifies the file system to use for the operation.
   If `opt` is provided, it is passed as an additional option to the
   operation."
  (gethash "list"
           (zr-rclone-rc-contact "operations/list"
                                 (json-encode `(("fs" . ,(concat fs ":"))
                                                ("remote" . ,remote)
                                                ("opt" . ,opt))))))

(defun zr-rclone-directory-files (fs remote &optional full)
  "Return a list of names of files in rclone remote."
  (mapcar (lambda (a)
            (let ((f (gethash "Path" a)))
              (if full (concat fs ":" f)
                (file-name-nondirectory f))))
          (zr-rclone-directory-files-internal
           fs remote '(("noModTime" . t) ("noMimeType" . t)))))

(defun zr-rclone-directory-files-recursively
    (fs remote regexp &optional include-directorys)
  "Return a list of names of files in rclone remote recursively."
  (cl-delete-if-not
   (lambda (a) (string-match-p regexp a))
   (mapcar (lambda (a) (format "%s:%s" fs (gethash "Path" a)))
           (zr-rclone-directory-files-internal
            fs remote
            (let ((opt '(("noModTime" . t) ("recurse" . t) ("noMimeType" . t))))
              (if include-directorys opt
                (cons'("filesOnly" . t) opt)))))))

(defun zr-rclone-copy-file
    (src-fs src-remote dst-fs dst-remote)
  "Copy a file from source remote to destination remote."
  (zr-rclone-rc-contact
   "operations/copyfile"
   (json-serialize
    `((srcFs . ,(if (file-directory-p src-fs)
                    (expand-file-name src-fs)
                  (concat src-fs ":")))
      (srcRemote . ,(zr-rclone-normalize-remote-path src-remote))
      (dstFs . ,(if (file-directory-p dst-fs)
                    (expand-file-name dst-fs)
                  (concat dst-fs ":")))
      (dstRemote . ,(zr-rclone-normalize-remote-path dst-remote))))))

(defun zr-rclone-filelist-export (fs remote &optional regexp prefix)
  (let ((filelist (mapcar (lambda (a)
                            (if prefix
                                (replace-regexp-in-string
                                 (format "^%s:" fs) prefix a)
                              a))
                          (zr-rclone-directory-files-recursively
                           fs remote (or regexp ".*")))))
    filelist))

(defvar zr-rclone-file-transform-alist nil
  "Alist of transformations to apply to files before play it.
Each element has the form (ORIG . REPLACEMENT), where ORIG is a regular
expression and REPLACEMENT is the replacement text.  Every element will
be tested in turn, allowing more than one transformation to be made.

Note that ORIG and REPLACEMENT are passed as arguments to
`string-match', so you can, for example, use match groups in ORIG and
backreferences in REPLACEMENT.")

(defun zr-rclone-transform-file-path (file)
  "Transform FILE path according to `zr-rclone-file-transform-alist'."
  (dolist (transform zr-rclone-file-transform-alist file)
    (when (string-match (car transform) file)
      (setq file (replace-match (cdr transform) nil nil file)))))

(defvar zr-rclone-playlist-history nil)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'zr-rclone-playlist-history))

(when (require 'emms nil t)
  (define-emms-source rclone (dir)
    "An EMMS source for rclone remote."
    (interactive (list (read-string "Play rclone directory: "
                                    nil 'zr-rclone-playlist-history)))
    (emms-playlist-ensure-playlist-buffer)
    (add-to-history 'zr-rclone-playlist-history dir 100)
    (let* ((parts (string-split dir ":"))
           (remote (car parts))
           (path (string-join (cdr parts)))
           (files (zr-rclone-directory-files-recursively
                   remote path
                   (rx (| (regexp (emms-source-file-regex))
                          (regexp (image-file-name-regexp)))))))
      (dolist (file files)
        (unless (string-match emms-source-file-exclude-regexp file)
	      (funcall emms-playlist-insert-track-function 
		           (emms-track 'url (zr-rclone-transform-file-path file))))))))

(defvar zr-rclone-mpv-args-history nil)
(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'zr-rclone-mpv-args-history))

(defvar zr-rclone-mpv-ipc-server
  (pcase system-type
    ('windows-nt "\\\\.\\pipe\\mpv-rclone")))

(defun zr-rlcone-mpv-m3u8-provide (files timeout)
  (let* ((m3u8-tempo
          (concat "HTTP/1.1 200 OK\r\n"
                  "Content-Type: application/vnd.apple.mpegurl\r\n"
                  "Content-Length: %s\r\n"
                  "\r\n#EXTM3U\r\n%s"))
         (proc (make-network-process
                :name "mpv-m3u8-provider"
                :server t
                :service t
                :sentinel
                (lambda (process event)
                  (cond
                   ((string-prefix-p "open from" event)
                    (process-send-string
                     process (format m3u8-tempo (length files) files))))))))
    (when (process-live-p proc)
      (unless (= timeout 0)
        (run-at-time timeout nil
                     (lambda ()
                       (delete-process proc)
                       (kill-buffer (process-buffer proc)))))
      proc)))

(defun zr-rclone-mpv-android-proc (files)
  (when-let*
      ((m3u8-server (zr-rlcone-mpv-m3u8-provide files 5))
       (data (format "http://127.0.0.1:%d"
                     (process-contact m3u8-server :service))))
    (call-process "termux-am" nil nil nil
                  "start" "-a" "android.intent.action.VIEW"
                  "-t" "video/any" "-p" "is.xyz.mpv.ytdl"
                  "-d" data)))

(defun zr-rclone-mpv-local-proc (files)
  (let* ((mpv-extra-args (read-shell-command "mpv " nil 'zr-rclone-mpv-args-history))
         (mpv-args `(,(concat "--input-ipc-server="
                              zr-rclone-mpv-ipc-server)
                     "--playlist=-" "--terminal=no"
                     ,@(split-string-shell-command mpv-extra-args))))
    (add-to-history 'zr-rclone-mpv-args-history mpv-extra-args 100)
    (if (bufferp files)
        (with-current-buffer files
          (apply #'call-process-region nil nil "mpv" nil 0 nil mpv-args))
      (apply #'call-process-region files nil "mpv" nil 0 nil mpv-args))))

(defun zr-rclone-mpv-remote-proc (files)
  (let* ((mpv-args (read-shell-command "mpv " nil 'zr-rclone-mpv-args-history))
         (url-request-extra-headers
          `(("Content-Type" . "application/vnd.apple.mpegurl")
            ("Origin" . ,(encode-coding-string
                          (concat "ssh://" (system-name)) 'utf-8))
            ("Authorization"
             . ,(encode-coding-string (auth-source-pick-first-password
                                       :host "mpv.nginx.localhost")
                                      'utf-8))
            ("args" . ,(encode-coding-string mpv-args 'utf-8))))
         (url-request-data (encode-coding-string files 'utf-8)))
    (add-to-history 'zr-rclone-mpv-args-history mpv-args 100)
    (url-retrieve "http://127.0.0.1:7780/lua/mpv" #'ignore nil t)))

(defvar zr-rclone-mpv-play-function
  (cond
   ((getenv "SSH_CONNECTION" (selected-frame)) #'zr-rclone-mpv-remote-proc)
   ((eq system-type 'android) #'zr-rclone-mpv-android-proc)
   (t #'zr-rclone-mpv-local-proc)))

(defun zr-rclone-mpv-play-dwim ()
  (interactive)
  (when-let*
      ((regexp (rx (| (regexp (emms-source-file-regex))
                      (regexp (image-file-name-regexp)))))
       (files
        (pcase major-mode
          ('dired-mode
           (string-join
            (mapcan
             (lambda (item)
               (mapcar #'zr-rclone-transform-file-path
                       (if (file-directory-p item)
                           (directory-files-recursively
                            item regexp nil nil t)
                         (list item))))
             (dired-get-marked-files))
            "\n"))
          ('emms-playlist-mode
           (if (use-region-p)
               (buffer-substring (region-beginning)
                                 (region-end))
             (buffer-string))))))
    (funcall zr-rclone-mpv-play-function files)))

(with-eval-after-load 'dired
  (bind-keys
   :map zr-dired-spc-prefix-map
   ("m" . zr-rclone-mpv-play-dwim)))

(with-eval-after-load 'emms-playlist-mode
  (bind-keys
   :map emms-playlist-mode-map
   ("m" . zr-rclone-mpv-play-dwim)))

(provide 'init-rclone)
;;; init-rclone.el ends here

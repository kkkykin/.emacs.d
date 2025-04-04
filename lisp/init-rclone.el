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

(provide 'init-rclone)
;;; init-rclone.el ends here

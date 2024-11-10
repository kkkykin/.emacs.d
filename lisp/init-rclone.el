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

(defcustom my/rclone-baseurl "http://127.0.0.1:5572"
  "Default rclone rc baseurl.")

(defcustom my/rclone-root-directory (pcase system-type
                                      ('windows-nt "d:/rclone/")
                                      ('gnu/linux (expand-file-name
                                                   "~/rclone/")))
  "Default rclone root directory."
  :type 'directory)

(defun my/rclone-rc-contact (action &optional args json-type async baseurl)
  "Contact with rclone."
  (with-current-buffer (get-buffer-create "*rclone-rc*")
    (when-let* ((begin (point-max))
                (url (url-generic-parse-url my/rclone-baseurl))
                (auth (car
                       (auth-source-search :max 1
                                           :host (url-host url)
                                           :port (url-port url))))
                (user (concat (plist-get auth :user)
                              ":" (auth-info-password auth))))
      (goto-char begin)
      (apply #'call-process "curl" nil (current-buffer) nil
             "-su" user "-XPOST"
             (concat (or baseurl my/rclone-baseurl) "/" action)
             (when args
               (append '("-H" "Content-Type: application/json" "-d")
                       (list args))))
      (with-restriction begin (point-max)
        (goto-char begin)
        (apply #'json-parse-buffer json-type)))))

;; [[elisp:(my/rclone-rc-contact "rc/noop" (json-encode '(("potato" . 1) ("sausage" . 2))))]]

;; [[elisp:(my/rclone-rc-contact "config/listremotes")]]

(defun my/rclone-list-remotes ()
  "Lists the remotes in the config file and defined in environment
variables."
  (gethash "remotes"
           (my/rclone-rc-contact "config/listremotes" nil
                                 '(:array-type list))))

(defun my/rclone-list-mounts ()
  "Show current mount points."
  (plist-get (my/rclone-rc-contact "mount/listmounts" nil
                                   '(:object-type plist))
             :mountPoints))

(defun my/rclone-mount-remote (remote &optional root)
  "Create a new mount point."
  (interactive
   (list (completing-read "Remote: "
                          (cl-delete-if
                           (lambda (r) (string-prefix-p "local" r))
                           (my/rclone-list-remotes)))))
  (let* ((root (or root my/rclone-root-directory))
         (path (expand-file-name remote root)))
    (make-directory root t)
    (my/rclone-rc-contact
     "mount/mount"
     (json-encode `(("fs" . ,(concat remote ":"))
                    ("mountPoint" . ,path)
                    ("vfsOpt" . ,(json-encode '(("vfs-cache-mode" . "writes")))))))
    (dired path)))

(defun my/rclone-unmount (point)
  "Unmount selected active mount."
  (interactive
   (list (completing-read "Point: "
                          (mapcar (lambda (m) (plist-get m :MountPoint))
                                  (my/rclone-list-mounts)))))
  (my/rclone-rc-contact
   "mount/unmount"
   (json-encode `(("mountPoint" . ,point)))))

(defun my/rclone-unmount-all ()
  "Unmount all active mounts."
  (interactive)
  (my/rclone-rc-contact "mount/unmountall"))

(defun my/rclone-quit ()
  "Exit rclone safely."
  (interactive)
  (my/rclone-rc-contact "core/quit"))

(defun my/rclone-directory-files-internal (fs remote &optional opt)
  "Returns a list of files in the remote directory specified by `remote`.
   The `fs` argument specifies the file system to use for the operation.
   If `opt` is provided, it is passed as an additional option to the
   operation."
  (gethash "list"
           (my/rclone-rc-contact "operations/list"
                                 (json-encode `(("fs" . ,(concat fs ":"))
                                                ("remote" . ,remote)
                                                ("opt" . ,opt))))))

(defun my/rclone-directory-files (fs remote &optional full)
  "Return a list of names of files in rclone remote."
  (mapcar (lambda (a)
            (let ((f (gethash "Path" a)))
              (if full (concat fs ":" f)
                (file-name-nondirectory f))))
          (my/rclone-directory-files-internal
           fs remote '(("noModTime" . t) ("noMimeType" . t)))))

(defun my/rclone-directory-files-recursively
    (fs remote regexp &optional include-directorys)
  "Return a list of names of files in rclone remote recursively."
  (cl-delete-if-not
   (lambda (a) (string-match-p regexp a))
   (mapcar (lambda (a) (format "%s:%s" fs (gethash "Path" a)))
           (my/rclone-directory-files-internal
            fs remote
            (let ((opt '(("noModTime" . t) ("recurse" . t) ("noMimeType" . t))))
              (if include-directorys opt
                (cons'("filesOnly" . t) opt)))))))

(defun my/rclone-filelist-export (fs remote file &optional regexp prefix)
  (write-region
   (mapconcat (lambda (a)
                (if prefix
                    (replace-regexp-in-string
                     (format "^%s:" fs) prefix a)
                  a))
              (my/rclone-directory-files-recursively
               fs remote (or regexp ".*"))
              "\n")
   nil file))

(provide 'init-rclone)
;;; init-rclone.el ends here

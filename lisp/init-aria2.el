;;; init-aria2.el --- aria2 related                  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  

;; Author:  <kkky@KKSBOW>
;; Keywords: files, tools

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

(require 'aria2)
(condition-case nil
    (setq aria2--cc (eieio-persistent-read aria2-cc-file aria2-controller))
  (error (setq aria2--cc (make-instance aria2-controller
                                        "aria2-controller"
                                        :file aria2-cc-file))))

(defvar z2/on-download-start-functions nil
  "Executed after download got started.")

(defvar z2/on-download-pause-functions nil
  "Executed after download was paused.")

(defvar z2/on-download-stop-functions nil
  "Executed after download got stopped.")

(defvar z2/on-download-complete-functions nil
  "Executed after download completed.")

(defvar z2/on-bt-download-complete-functions nil
  "Called after download completed and seeding is over.")

(defvar z2/on-download-error-functions nil
  "Executed after download aborted due to error.")

(defun z2/event-handler (event gid count &optional file)
  (let ((hook (intern (format "zr-aria2-on-%s-functions" event)))
        info)
    (pcase event
      ("download-start"
       (push (cons 'uris (getUris aria2--cc gid)) info))
      ("download-complete"
       (push (cons 'file file) info)
       (push (cons 'count count) info)))
    (run-hook-with-args hook gid info)))

(defvar z2/limited-server-regexp
  (rx bos "pixeldrain.com" eos))

(defvar z2/option-changed-gid nil)

(defun z2/change-option-for-uri (gid info)
  (unless (member gid z2/option-changed-gid)
    (let* ((uris (alist-get 'uris info))
           (used (cl-find-if
                  (lambda (u) (string= "used" (alist-get 'status u)))
                  uris))
           (uri (alist-get 'uri used))
           (urlobj (url-generic-parse-url uri))
           (host (url-host urlobj))
           (proxy (zr-net-match-proxy-rule urlobj host))
           opts)
      (when (string-match-p z2/limited-server-regexp host)
        (push '(max-connection-per-server . "1") opts))
      (unless (string= "DIRECT" proxy)
        (let ((ps (replace-regexp-in-string "^\\(SOCKS5\\|PROXY\\) "
                                            "http://" proxy t)))
          (push `(all-proxy . ,ps) opts)))
      (push gid z2/option-changed-gid)
      (when opts
        (pause aria2--cc gid)
        (changeOption aria2--cc gid opts)
        (unpause aria2--cc gid)))))
(add-hook 'z2/on-download-start-functions #'z2/change-option-for-uri)

(defun z2/remove-finished-gid (gid &rest _)
  (setq z2/option-changed-gid (delete gid z2/option-changed-gid)))
(add-hook 'z2/on-download-complete-functions #'z2/remove-finished-gid)
(add-hook 'z2/on-bt-download-complete-functions #'z2/remove-finished-gid)

(provide 'init-aria2)
;;; init-aria2.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("z2/" . "zr-aria2-"))
;; End:

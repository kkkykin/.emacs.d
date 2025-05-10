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
  (require 'aria2)
  (let ((hook (intern (format "zr-aria2-on-%s-functions" event)))
        (args (pcase event
                ("download-start"
                 (list (getUris aria2--cc gid))))))
    (apply #'run-hook-with-args hook gid args)))

(defvar z2/limited-server-regexp
  (rx bos "pixeldrain.com" eos))

(defun z2/change-option-for-uri (gid uris)
  (let* ((used (cl-find-if
                (lambda (u) (string= "used" (alist-get 'status u)))
                uris))
         (uri (alist-get 'uri used))
         (urlobj (url-generic-parse-url uri))
         (host (url-host urlobj))
         (proxy (zr-net-match-proxy-rule urlobj host))
         opts)
    (when (string-match-p z2/limited-server-regexp host)
      (push '(max-connection-per-server . 1) opts))
    (unless (string= "DIRECT" proxy)
      (let ((ps (replace-regexp-in-string "^\\(SOCKS5\\|PROXY\\) "
                                          "http://" proxy)))
        (push `(all-proxy . ,ps) opts)))
    (changeOption aria2--cc gid opts)))
(add-hook 'z2/on-download-start-functions #'z2/change-option-for-uri)

(provide 'init-aria2)
;;; init-aria2.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("z2/" . "zr-aria2-"))
;; End:

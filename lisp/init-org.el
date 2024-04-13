;;; init-org.el --- Org related                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <kkky@KKSBOW>
;; Keywords: extensions

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

(defun org-dblock-write:file-finder (params)
  "Create a dynamic block for listing all matched files in a directory and
its subdirectories."
  (let* ((dir (plist-get params :dir))
         (re (plist-get params :re))
         (files (directory-files-recursively dir re))
         (table-header "| Path |\n|------|\n")
         (table-rows (mapconcat (lambda (file) (format "| %s |" file)) files "\n"))
         (table (concat table-header table-rows)))
    (insert table)))

(defun my/org-open-link-nearby (&optional arg)
  "Follow a link nearby like Org mode does."
  (interactive "P")
  (unless (when (null arg)
            (ignore-error user-error
              (org-open-at-point-global)))
    (let ((count (pcase arg
                   ('- -1)
                   ((guard (numberp arg)) arg)
                   (_ 1)))
          link)
      (save-excursion
        (re-search-forward org-link-any-re
                           (when (use-region-p)
                             (if (natnump count)
                                 (region-end)
                               (region-beginning)))
                           t count)
        (setq link (match-string-no-properties 0))
        (if (and (string-match-p org-link-any-re link)
                 (y-or-n-p (format "Open link: %s?" link)))
            (org-link-open-from-string link)
          (user-error "No link found"))))))
(with-eval-after-load 'viper
  (keymap-substitute viper-vi-global-user-map
                     'org-open-at-point-global
                     'my/org-open-link-nearby))

(provide 'init-org)
;;; init-org.el ends here

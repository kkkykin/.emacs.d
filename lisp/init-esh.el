;;; init-esh.el --- eshell setup                     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  

;; Author:  <kw@comhb>
;; Keywords: terminals

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

(defun eshell/import-bookmark (&rest args)
  "Set env from files of bookmarks."
  (require 'bookmark)
  (dolist (mark bookmark-alist)
    (when-let* ((file (bookmark-get-filename mark))
                ((not (bookmark-get-handler mark))))
      (eshell-set-variable
       (concat (string-join args "_") "_" (car mark)) file))))

(provide 'init-esh)
;;; init-esh.el ends here

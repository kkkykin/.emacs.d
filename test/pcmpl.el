;;; pcmpl.el --- pcmpl ert                           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <kkky@KKSBOW>
;; Keywords: help

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

(ert-deftest test/advice-pcomplete-from-help ()
  "Tester for `my/advice-pcomplete-from-help'."
  (should (equal '("git" "branch" "-h")
                 (my/advice-pcomplete-from-help
                  `(,vc-git-program "help" "branch")))))

(provide 'pcmpl)
;;; pcmpl.el ends here

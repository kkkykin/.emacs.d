;;; init-pcmpl.el --- Completions for some tools             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Keywords: abbrev, convenience, extensions, terminals, tools

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

;; adb, 7z completion.

;;; Code:

(defun pcomplete/7z ()
  "Completion for `7z'."
  (let ((subcommands (pcomplete-from-help `(,archive-7z-program "--help")
                                          :argument "[[:alpha:]]+"))
        (switches (pcomplete-from-help `(,archive-7z-program "--help"))))
    (while (not (member (pcomplete-arg 1) subcommands))
      (pcomplete-here subcommands))
    (let ((subcmd (pcomplete-arg 1))
          (cur (pcomplete-arg)))
      (while (pcase subcmd
               ;; ((guard (string-prefix-p "-o" cur))
               ;;  (pcomplete-here
               ;;   (directory-files )))
               ((guard (string= "-" cur))
                (pcomplete-here switches))
               ((or "a" "x") (pcomplete-here (pcomplete-entries)))))
      ))
  )
(defalias 'pcomplete/7zz 'pcomplete/7z)

(defun pcomplete/adb ()
  "Completion for `adb'."
  (message (pcomplete-arg))
  (let ((subcommands (pcomplete-from-help "adb --help"
                                          :margin "^\\( \\)[a-z]"
                                          :argument "[[:alpha:]-]+")))
    (while (not (member (pcomplete-arg 1) subcommands))
      (if (string-prefix-p "-" (pcomplete-arg))
          (pcomplete-here (pcomplete-from-help "adb --help"
                                               :margin "^\\( \\)-"))
        (pcomplete-here (completion-table-merge
                         subcommands
                         (when (string-prefix-p "-" (pcomplete-arg 1))
                           (pcomplete-entries))))))
    (let ((subcmd (pcomplete-arg 1)))
      (while (pcase subcmd
               ((guard (string-prefix-p "-" (pcomplete-arg)))
                (pcomplete-here
                 (pcomplete-from-help
                  "adb --help"
                  :argument "'?\\(--?[[:alpha:]-]+\\)'?[ :]"
                  :narrow-start (format "^ %s "
                                        (pcase subcmd
                                          ((or "install" 
                                               "install-multiple")
                                           "install-multi-package")
                                          (_ subcmd)))
                  :narrow-end "^ [[:alpha:]-]+ ")))
               ((or "install" "install-multiple" "install-multi-package")
                (pcomplete-here
                 (pcomplete-entries ".+\\(\.apks?\\|/\\)" )))
               ("pull"
                (pcomplete-here
                 (process-lines "adb" "shell" "-nT" "ls" "-d1"
                                (replace-regexp-in-string
                                 "\\(.+/\\)[^/]+\\'" "\"'\\1\'**\""
                                 (pcomplete-arg))))))))))

(provide 'init-pcmpl)
;;; init-pcmpl.el ends here


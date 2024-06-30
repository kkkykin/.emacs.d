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

(defun my/pcmpl-7z--list-archive ()
  "List archive files with 7z."
  (let ((pass (or (seq-some (lambda (a) (and (string-prefix-p "-p" a) a))
                            pcomplete-args)
                  "-p⑨"))
        (arc (seq-some (lambda (a) (and (file-regular-p a) a)) pcomplete-args))
        files)
    (when arc
      (with-temp-buffer
        (call-process archive-7z-program nil t t "l" arc pass)
        (goto-char (point-min))
        (while (re-search-forward "^[[:digit:]-: ]\\{20\\}[[:upper:].]+ +[[:digit:]]+[[:digit:] ]\\{15\\}\\(.+\\)" nil t)
          (push (match-string 1) files))))
    files))

(defun pcomplete/7z ()
  "Completion for `7z'."
  (let ((subcommands (pcomplete-from-help `(,archive-7z-program "--help")
                                          :argument "[[:alpha:]]+"))
        (switches (pcomplete-from-help `(,archive-7z-program "--help"))))
    (while (not (member (pcomplete-arg 1) subcommands))
      (pcomplete-here subcommands))
    (let ((subcmd (pcomplete-arg 1)))
      (while
          (let ((cur (pcomplete-arg)))
            (pcase subcmd
              ((guard (string-prefix-p "-" cur))
               (cond ((string-prefix-p "-o" cur)
                      (let ((path (substring cur 2)))
                        (pcomplete-here
                         (mapcar (lambda (a) (concat "-o" a))
                                 (directory-files
                                  (or (file-name-directory path) "")
                                  t)))))
                     (t (pcomplete-here switches))))
              ((or "a" "h" "l" "t" "x")
               (pcomplete-here (pcomplete-entries)))
              ((or "d" "e" "rn" "u")
               (pcomplete-here (completion-table-merge
                                (pcomplete-entries)
                                (my/pcmpl-7z--list-archive))))))))))
(defalias 'pcomplete/7zz 'pcomplete/7z)

(defun pcomplete/adb ()
  "Completion for `adb'."
  (let ((subcommands (pcomplete-from-help "adb --help"
                                          :margin "^\\( \\)[a-z]"
                                          :argument "[[:alpha:]-]+"))
        (switches (pcomplete-from-help "adb --help" :margin "^\\( \\)-"))
        (devices (mapcar (lambda (a) (replace-regexp-in-string "\t.+" "" a))
                         (delete "" (cdr (process-lines "adb" "devices"))))))
    (while (not (member (pcomplete-arg 1) subcommands))
      (cond ((string= "-s" (pcomplete-arg 1)) (pcomplete-here devices))
            ((string-prefix-p "-" (pcomplete-arg))
             (pcomplete-here switches))
            (t (pcomplete-here (completion-table-merge
                                subcommands
                                (when (string-prefix-p "-" (pcomplete-arg 1))
                                  (pcomplete-entries)))))))
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

(defun pcomplete/fd ()
  "Completion for `fd'."
  (pcomplete-here-using-help "fd --help"))

(provide 'init-pcmpl)
;;; init-pcmpl.el ends here

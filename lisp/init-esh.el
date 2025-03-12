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

(defun eshell/with-editor-maybe (&rest args)
  "Export EDITOR env."
  (unless (memq 'with-editor-export-editor eshell-mode-hook)
    (eshell-set-variable "EDITOR" (car args))))

(defun eshell/vi (&rest args)
  "Open files in Neovim, optionally using a remote server if configured.
If `zr-viper-default-nvim-server' is bound and non-nil, files are opened
in the specified Neovim server. Otherwise, files are opened in a new
Neovim instance."
  (pcase system-type
    ('windows-nt (zr-win-display-windows-terminal)))
  (apply #'call-process "nvim" nil 0 nil
         (if (bound-and-true-p zr-viper-default-nvim-server)
             `("--server" ,zr-viper-default-nvim-server "--remote-tab"
               ,@(mapcar #'expand-file-name args))
           args)))

(defun eshell/vt (&rest args)
  "Open a new terminal buffer in Neovim with the specified command.
If `zr-viper-default-nvim-server' is bound and true, the command is
executed remotely in the existing Neovim server. Otherwise, it opens a
new Neovim instance.

ARGS: A list of arguments to be passed as the command to execute in the
terminal."
  (let ((term (concat "te " (combine-and-quote-strings (flatten-tree args)))))
    (apply #'call-process "nvim" nil 0 nil
           (if (bound-and-true-p zr-viper-default-nvim-server)
               `("--server" ,zr-viper-default-nvim-server "--remote-expr"
                 ,(format "execute('tabe | tc %s | %s')"
                          (if (file-remote-p default-directory) ""
                            default-directory)
                          (string-replace "'" "''" term)))
             (list "--cmd" term)))))

(defun ze/invoke-by-nvim (&rest args)
  "Invoke program in neovim."
  (pcase system-type
    ('windows-nt (zr-win-display-windows-terminal)))
  (throw 'eshell-replace-command
         (eshell-parse-command "vt" args)))

(dolist (cmd (list (rx bos (| "ping") eos)))
  (add-to-list 'eshell-interpreter-alist (cons cmd 'ze/invoke-by-nvim)))

(provide 'init-esh)
;;; init-esh.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ze/" . "zr-eshell-"))
;; End:

;;; init-comint.el --- Comint related                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <kkky@KKSBOW>
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


;; shell

(defun my/shell-setup ()
  "Setup various shell in shell-mode."
  (pcase (file-name-base (or explicit-shell-file-name shell-file-name))
    ("bash" (shell-dirtrack-mode -1))
    ("cmdproxy"
     (shell-dirtrack-mode -1)
     (dirtrack-mode)
     (setq dirtrack-list '("^\\([a-zA-Z]:.*\\)>" 1))
     (add-hook 'comint-preoutput-filter-functions
               (lambda (output)
                 (if (member (my/win-cmdproxy-real-program-name
                              (comint-previous-input-string 0))
                             '("whoami"))
                     output
                   (replace-regexp-in-string "\\`.*?\n" "" output)))
               -100 t))))
(add-hook 'shell-mode-hook #'my/shell-setup)



(provide 'init-comint)
;;; init-comint.el ends here

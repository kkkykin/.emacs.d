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

(defcustom zr-comint-dir (locate-user-emacs-file "comint/")
  "Directory where comint saves data."
  :type 'directory
  :group 'my)

(defun zr-shell-setup ()
  "Setup various shell in shell-mode."
  (pcase (file-name-base (or explicit-shell-file-name shell-file-name))
    ("bash" (shell-dirtrack-mode -1))
    ("cmdproxy"
     (shell-dirtrack-mode -1)
     (dirtrack-mode)
     (setq dirtrack-list '("^\\([a-zA-Z]:.*\\)>" 1)))))
(add-hook 'shell-mode-hook #'zr-shell-setup)

(defun zr-advice-comint-save-history-sentinel (proc event)
  "Save history if the default sentinel not save, then kill buffer."
  (when-let* ((buf (process-buffer proc))
              (livep (buffer-live-p buf)))
    (with-current-buffer buf
      (unless (or (null comint-input-ring-file-name)
                  (zr-file-modified-recently-p comint-input-ring-file-name 5))
        (comint-write-input-ring))
      (when (string= event "finished\n")
        (kill-current-buffer)))))

(defun zr-comint-save-history ()
  "Let all comint-mode save `input-ring' history cross session."
  (when-let* ((proc (get-buffer-process (current-buffer)))
              (program (car (process-command proc)))
              (base (file-name-base program)))
    (setq-local comint-input-ring-file-name
                (expand-file-name base zr-comint-dir))
    (comint-read-input-ring t)
    (add-function :after (process-sentinel proc)
                  #'zr-advice-comint-save-history-sentinel)))
(add-hook 'comint-exec-hook #'zr-comint-save-history)



(provide 'init-comint)
;;; init-comint.el ends here

;;; init-win.el --- windows setup                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024  

;; Author:  <kkky@KKSBOW>
;; Keywords: local

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

(defun my/advice-shell-command-coding-fix (orig-fun &rest args)
  "Fix coding system for cmdproxy shell."
  (if-let* ((command (car args))
            (program-name
             (seq-some
              (lambda (x) (and (string-match-p "^[^\\(start\\|/.+\\)]" x) x))
              (split-string-shell-command command)))
            (need-fix (or (member program-name
                                  `( ,find-program ,grep-program
                                     "busybox" "curl" "ffmpeg" "git"
                                     "make" "mpv"))
                          (string= "compilation" command)
                          (string= "*Find*" command)
                          (string= "grep" command))))
      (let ((process-coding-system-alist
             `(("cmdproxy" utf-8 . ,locale-coding-system))))
        (apply orig-fun args))
    (apply orig-fun args)))

(defun my/output-coding-system-fix (input output)
  "Fix coding system by `process-coding-system-alist'."
  (if-let* ((program (car (split-string-shell-command input)))
            (coding-system (alist-get program process-coding-system-alist
                                      nil nil 'equal)))
      (if (listp coding-system)
          (decode-coding-string
           (encode-coding-string
            (decode-coding-string output (car coding-system))
            (cdr coding-system))
           'prefer-utf-8)
        (decode-coding-string output 'prefer-utf-8))
    output))

(defun my/eshell-coding-system-fix (output)
  "Fix stdout coding-system in eshell."
  (my/output-coding-system-fix (eshell-previous-input-string 0)
                               output))
;; (add-hook 'eshell-preoutput-filter-functions #'my/eshell-coding-system-fix nil)
;; (remove-hook 'eshell-preoutput-filter-functions #'my/eshell-coding-system-fix nil)

(defun my/shell-coding-system-fix (output)
  "Fix stdout coding-system in shell."
  (my/output-coding-system-fix (comint-previous-input-string 0) output))

(defun my/compilation-coding-system-fix (output)
  "Fix stdout coding-system in compilation."
  (let ((fixed (my/output-coding-system-fix
                (car compilation-arguments)
                (buffer-substring compilation-filter-start (point)))))
    (delete-region compilation-filter-start (point))
    (insert fixed)))
;; (with-eval-after-load 'compile
;;   (add-hook 'compilation-filter-hook #'my/compilation-coding-system-fix))

(defun my/advice-dired-shell-stuff-it (args)
  "Fix `;' cannot sequentially execute command on windows."
  (when-let* ((cmd (car args))
              (fix-p (string-match-p ";[ \t]*&?[ \t]*\\'" cmd))) 
    (setcar args
            (replace-regexp-in-string "\\(.*\\);[ \t]*\\(&?[ \t]*\\)\\'"
                                      "/wait \\1\\2" cmd)))
  args)

(defun my/run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\system32\\bash.exe"))
    (shell "*bash*")))

(defun my/toggle-shell ()
  "Toggle shell between wsl bash and cmd"
  (interactive)
  (if (string= shell-file-name "C:\\Windows\\system32\\bash.exe")
      (setq shell-file-name my/vanilla-shell)
    (setq my/vanilla-shell shell-file-name
          shell-file-name "C:\\Windows\\system32\\bash.exe")))

(dolist (fn '(shell-command start-file-process-shell-command))
  (advice-add fn :around #'my/advice-shell-command-coding-fix))

(add-hook 'shell-mode-hook
          (lambda ()
            (add-hook 'comint-preoutput-filter-functions
                      #'my/shell-coding-system-fix nil t)))

(setq default-process-coding-system '(utf-8-dos . utf-8-unix) ;; change this maybe break tramp sshx
      process-coding-system-alist
      `(("cmdproxy" . ,locale-coding-system)
        ("git" utf-8 . ,locale-coding-system)
        ("sha256sum" utf-8 . ,locale-coding-system))
      file-name-coding-system locale-coding-system
      shr-use-fonts nil)

(setenv "HOME" (file-name-parent-directory user-emacs-directory))

(add-to-list 'exec-suffixes ".ps1")

(provide 'init-winnt)
;;; init-winnt.el ends here

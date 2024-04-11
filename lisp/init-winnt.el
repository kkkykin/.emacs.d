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

(defun mw/find-shell-command-coding-system (command)
  "Find coding system for shell command."
  (let ((program (seq-some (lambda (x) (and (not (string-prefix-p "/" x))
                                        (not (string= "start" x)) x))
                           (split-string-shell-command command))))
    (find-operation-coding-system 'call-process program)))

(defun mw/advice-shell-command-coding-fix (orig-fun &rest args)
  "Fix coding system for by change I/O coding-system."
  (let* ((coding-system (mw/find-shell-command-coding-system (car args)))
         (coding-system-for-read (car coding-system))
         (coding-system-for-write (cdr coding-system)))
    (apply orig-fun args)))
(advice-add 'shell-command :around #'mw/advice-shell-command-coding-fix)

(defun mw/output-coding-system-fix (input output)
  "Fix coding system by convert string."
  (if-let ((coding-system (mw/find-shell-command-coding-system input)))
      (decode-coding-string
       (encode-coding-string
        (decode-coding-string output (car coding-system))
        (cdr coding-system))
       'prefer-utf-8)
    output))

(defun mw/eshell-coding-system-fix (output)
  "Fix stdout coding-system in eshell."
  (if-let ((promptp (not (string-match-p
                          (concat eshell-prompt-regexp "$") output)))
           (command (condition-case nil
                        (eshell-previous-input-string 0)
                      (error nil))))
      (mw/output-coding-system-fix command output)
    output))
(add-hook 'eshell-preoutput-filter-functions #'mw/eshell-coding-system-fix nil)

(defun mw/shell-coding-system-fix (output)
  "Fix stdout coding-system in shell."
  (mw/output-coding-system-fix
   (comint-previous-input-string 0)
   output))

(defun mw/shell-mode-setup ()
  "Setup for shell-mode."
  (add-hook 'comint-preoutput-filter-functions
            #'mw/shell-coding-system-fix nil t))
(add-hook 'shell-mode-hook #'mw/shell-mode-setup)

(defun mw/compilation-coding-system-fix ()
  "Fix stdout coding-system in compilation."
  (let ((coding-system (mw/find-shell-command-coding-system
                        (car compilation-arguments))))
    (setq-local coding-system-for-read (car coding-system)
                coding-system-for-write (cdr coding-system))))
(setq compilation-process-setup-function #'mw/compilation-coding-system-fix)

(defun mw/grep-coding-system-fix ()
  "Fix coding-system for grep."
  (let ((coding-system (mw/find-shell-command-coding-system grep-program)))
    (setq-local coding-system-for-read (car coding-system))))
(add-hook 'grep-setup-hook #'mw/grep-coding-system-fix)

(defun mw/find-dired-coding-system-fix ()
  "Fix coding-system for find-dired."
  (let ((fixed (mw/output-coding-system-fix find-program (buffer-string))))
    (delete-region (point-min) (point-max))
    (insert fixed)))
(with-eval-after-load 'find-dired
  (let ((ori find-dired-refine-function))
    (setq find-dired-refine-function 'mw/find-dired-coding-system-fix)
    (advice-add find-dired-refine-function :after ori)))

(defun mw/advice-dired-shell-stuff-it (args)
  "Fix `;' cannot sequentially execute command on windows."
  (when-let* ((cmd (car args))
              (fix-p (string-match-p ";[ \t]*&?[ \t]*\\'" cmd))) 
    (setcar args
            (replace-regexp-in-string "\\(.*\\);[ \t]*\\(&?[ \t]*\\)\\'"
                                      "/wait \\1\\2" cmd)))
  args)
(with-eval-after-load 'dired-aux
  (advice-add 'dired-shell-stuff-it :filter-args #'mw/advice-dired-shell-stuff-it))

(defun mw/run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\system32\\bash.exe"))
    (shell "*bash*")))

(defun mw/toggle-shell ()
  "Toggle shell between wsl bash and cmd"
  (interactive)
  (if (string= shell-file-name "C:\\Windows\\system32\\bash.exe")
      (setq shell-file-name mw/vanilla-shell)
    (setq mw/vanilla-shell shell-file-name
          shell-file-name "C:\\Windows\\system32\\bash.exe")))

(setq grep-program "ug"
      grep-use-null-device nil
      grep-highlight-matches t
      grep-find-command
      '("fd -t f -X ug --color=auto -nH --null -e \"\" {} ;" . 43)
      grep-find-template
      "fd --base-directory <D> <X> -t f <F> -X ug <C> -nH --null -e <R> {} ;"
      find-program "fd"
      default-process-coding-system '(utf-8-dos . utf-8-unix) ;; change this maybe break tramp sshx
      process-coding-system-alist
      `(("cmdproxy" . ,locale-coding-system)
        ("curl" utf-8 . ,locale-coding-system)
        ("ffmpeg" utf-8 . ,locale-coding-system)
        (,find-program utf-8 . ,locale-coding-system)
        (,grep-program utf-8 . ,locale-coding-system)
        ("git" utf-8 . ,locale-coding-system)
        ("make" utf-8 . ,locale-coding-system)
        ("mpv" utf-8 . ,locale-coding-system)
        ("sha256sum" utf-8 . ,locale-coding-system))
      file-name-coding-system locale-coding-system
      shr-use-fonts nil)

(setenv "HOME" (file-name-parent-directory user-emacs-directory))

(add-to-list 'exec-suffixes ".ps1")

(provide 'init-winnt)
;;; init-winnt.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mw/" . "my/win-"))
;; End:

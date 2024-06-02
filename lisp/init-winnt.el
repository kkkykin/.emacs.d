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

(require 'cl-lib)


;; UWP

(defun mw/uwp-visit-localhost (app)
  "Make a UWP App visist localhost."
  (interactive
   (list
    (completing-read "App: "
                     (cl-delete-if
                      (lambda (a) (member a '("." "..")))
                      (directory-files
                       (substitute-in-file-name
                        "$USERPROFILE/AppData/Local/Packages"))))))
  (call-process "CheckNetIsolation" nil nil nil
                "LoopbackExempt" "-a" (concat "-n=" app)))


;; shell

(defconst mw/vanilla-shell shell-file-name)

(defun mw/cmdproxy-real-program-name (cmd)
  "Find invoked real program name in cmd."
  (seq-some (lambda (x) (and (not (string-prefix-p "/" x))
                         (not (string= "start" x)) x))
            (split-string-shell-command cmd)))

(defun mw/find-shell-command-coding-system (command)
  "Find coding system for shell command."
  (let ((program (mw/cmdproxy-real-program-name command)))
    (find-operation-coding-system 'call-process program)))

(define-advice shell-command (:around (orig-fun &rest args) fix-coding)
  (let* ((coding-system (mw/find-shell-command-coding-system (car args)))
         (coding-system-for-read (car coding-system))
         (coding-system-for-write (cdr coding-system)))
    (apply orig-fun args)))

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
  (unless (file-remote-p default-directory)
    (add-hook 'comint-preoutput-filter-functions
              #'mw/shell-coding-system-fix nil t)))
(add-hook 'shell-mode-hook #'mw/shell-mode-setup)

(defun mw/run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\system32\\bash.exe"))
    (shell "*bash*")))

(defun mw/toggle-shell ()
  "Toggle shell between wsl bash and cmd"
  (interactive)
  (setq shell-file-name
        (if (string= shell-file-name mw/vanilla-shell)
            "C:/Windows/system32/bash.exe"
          mw/vanilla-shell)))

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


;; dired

(with-eval-after-load 'dired-aux
  (dolist (item `(("\\.exe\\'" .
                   ,(let ((cab (string-replace "/" "\\" (concat temporary-file-directory "cab-" (md5 (system-name))))))
                      (format "makecab %%i %s && copy /b/y \"%s\"+\"%s\" %%o & del /q/f \"%s\""
                              cab (string-replace "/" "\\" (executable-find "extrac32")) cab cab)))))
    (add-to-list 'dired-compress-files-alist item))
  (define-advice dired-shell-stuff-it (:filter-args (args) fix-seqentially-exec)
    "Fix `;' cannot sequentially execute command on windows."
    (when-let* ((cmd (car args))
                (fix-p (string-match-p ";[ \t]*&?[ \t]*\\'" cmd)))
      (setcar args
              (replace-regexp-in-string "\\(.*\\);[ \t]*\\(&?[ \t]*\\)\\'"
                                        "/wait \\1\\2" cmd)))
    args))


;; tramp

(with-eval-after-load 'tramp
  (setq tramp-default-method "sshx"
        tramp-use-connection-share nil))

(setq grep-program "ug"
      grep-use-null-device nil
      grep-highlight-matches t
      find-program "fd"
      default-process-coding-system '(utf-8-dos . utf-8-unix) ;; change this maybe break tramp sshx
      process-coding-system-alist
      `(("cmdproxy" . ,locale-coding-system)
        ("curl" utf-8 . ,locale-coding-system)
        ("ffmpeg" utf-8 . ,locale-coding-system)
        (,find-program utf-8 . ,locale-coding-system)
        (,grep-program utf-8 . ,locale-coding-system)
        ("git" utf-8 . ,locale-coding-system)
        ("ls" utf-8 . ,locale-coding-system)
        ("make" utf-8 . ,locale-coding-system)
        ("mpv" utf-8 . ,locale-coding-system)
        ("sha256sum" utf-8 . ,locale-coding-system))
      file-name-coding-system locale-coding-system
      shr-use-fonts nil)

(setenv "HOME" (file-name-parent-directory user-emacs-directory))

(add-to-list 'exec-suffixes ".ps1")


;; viper

(with-eval-after-load 'viper
  (add-hook 'viper-vi-state-hook (lambda () (w32-set-ime-open-status nil))))


;; pcmpl

(with-eval-after-load 'pcmpl-git
  (define-advice pcomplete-from-help (:filter-args (args) fix-git-help)
    "Full document of `git' not work on windows."
    (let ((cmd (car args)))
      (when (and (equal `(,vc-git-program "help") (seq-take cmd 2))
                 (not (string= "-a" (caddr cmd))))
        (setcar args (list vc-git-program (elt cmd 2) "-h"))))
    args))

(provide 'init-winnt)
;;; init-winnt.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mw/" . "my/win-"))
;; End:

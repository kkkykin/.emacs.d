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
(require 'derived)


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

(define-advice shell-command-on-region (:around (orig-fun &rest args) fix-coding)
  "Fix coding system for `shell-command-on-region' when not in a remote
directory."
  (if (file-remote-p default-directory)
      (apply orig-fun args)
    (let ((process-coding-system-alist
           `(("cmdproxy" ,@(mw/find-shell-command-coding-system (caddr args))))))
      (apply orig-fun args))))

(with-eval-after-load 'ob-eval
  (define-advice org-babel--shell-command-on-region (:around (orig-fun &rest args) fix-coding)
    "Fix coding system for `org-babel--shell-command-on-region' when not in a
remote directory."
    (if (file-remote-p default-directory)
        (apply orig-fun args)
      (let ((process-coding-system-alist
             `(("cmdproxy" ,@(mw/find-shell-command-coding-system (car args))))))
        (apply orig-fun args)))))

(defun mw/proc-coding-system-fix (&optional proc cmd)
  "Fix the coding system for a process based on its command.

This function sets the coding system for the process PROC based on the
command it is running.  It only applies the fix if the current
`default-directory' is local (i.e., not remote).

- If PROC is not provided, it defaults to the process associated with
  the current buffer.

- If CMD is provided, it use for determining the coding system.
  By default, it uses the third element (index 2) of the process
  command list."
  (if-let* (((not (file-remote-p default-directory)))
            (proc (or proc (get-buffer-process (current-buffer))))
            (cs (mw/find-shell-command-coding-system
                 (or cmd (nth 2 (process-command proc))))))
      (set-process-coding-system proc (car cs) (cdr cs))
    t))

(dolist (h `(,(derived-mode-hook-name async-shell-command-mode)
             compilation-start-hook))
  (add-hook h #'mw/proc-coding-system-fix))

(defun mw/output-coding-system-fix (cmd output)
  "Fix coding system by convert string."
  (if-let ((localp (not (file-remote-p default-directory)))
           (cs (mw/find-shell-command-coding-system cmd)))
      (decode-coding-string (encode-coding-string output (cdr cs)) (car cs))
    output))

(defun mw/eshell-change-cs-when-exec (proc)
  "Change the coding system of the Eshell process PROC based on the command
being executed.

This function checks if the current directory is local (not remote). If
so, it determines the appropriate coding system for the command being
executed by the process PROC using `find-operation-coding-system`.  It
then sets the process coding system for PROC to ensure correct encoding
and decoding of input and output.

The function is intended to be used with `eshell-exec-hook' to
dynamically adjust the coding system for each command executed in
Eshell."
  (when-let (((not (file-remote-p default-directory)))
             (cs (find-operation-coding-system
                  #'call-process (car (process-command proc)))))
    (set-process-coding-system proc (car cs) (cdr cs))))
(add-hook 'eshell-exec-hook #'mw/eshell-change-cs-when-exec)

(defun mw/shell-change-cs-before-send-input (proc string)
  "See `mw/eshell-change-cs-when-exec'."
  (when-let (((not (string-empty-p string)))
             ((mw/proc-coding-system-fix proc string))
             (cs (find-operation-coding-system
                  #'call-process (car (process-command proc)))))
    (set-process-coding-system proc (car cs) (cdr cs)))
  (comint-simple-send proc string))

(defun mw/shell-mode-setup ()
  "Setup for shell-mode."
  (when (string-match-p "cmdproxy"
                        (or explicit-shell-file-name shell-file-name))
    (setq comint-process-echoes t
          comint-input-sender #'mw/shell-change-cs-before-send-input)))
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

(defun mw/coding-conv-region (min from to)
  "Convert coding system between min and `point-max'."
  (unless (file-remote-p default-directory)
    (encode-coding-region min (point-max) from)
    (decode-coding-region min (point-max) to)))


;; dired

(define-advice insert-directory (:around (orig-fun &rest args) fix-cs)
  "Force decode `ls' output with 'utf-8."
  (let ((coding-system-for-read 'utf-8))
    (apply orig-fun args)))

(with-eval-after-load 'dired
  (bind-keys
   :map dired-mode-map
   ("N" . woman-dired-find-file)))

(with-eval-after-load 'dired-aux
  (dolist (item `(("\\.exe\\'" .
                   ,(let ((cab (string-replace "/" "\\" (concat temporary-file-directory "cab-" (md5 (system-name))))))
                      (format "makecab %%i %s && copy /b/y \"%s\"+\"%s\" %%o & del /q/f \"%s\""
                              cab (string-replace "/" "\\" (executable-find "extrac32")) cab cab)))))
    (add-to-list 'dired-compress-files-alist item))
  (define-advice dired-shell-stuff-it (:filter-args (args) fix-seqentially-exec)
    "Fix `;' cannot sequentially execute command on windows."
    (when-let* ((localp (not (file-remote-p default-directory)))
                (cmd (car args)))
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
      find-ls-option '("-I -X ls -ldh {} ; | iconv -f utf-8 -t gb18030 -cs" . "-ldh")
      ls-lisp-use-insert-directory-program t
      default-process-coding-system '(utf-8-dos . utf-8-unix) ;; change this maybe break tramp sshx
      ispell-extra-args (list "--filter-path" (substitute-in-file-name "$USERPROFILE/scoop/apps/aspell/current/lib/aspell-0.60"))
      Info-default-directory-list
      (mapcar (lambda (a) (concat (getenv "USERPROFILE") "/scoop/apps/" a))
              '("aspell/current/share/info"
                "mingw-winlibs-llvm-ucrt/current/share/info"))
      process-coding-system-alist
      `(("cmdproxy" . ,locale-coding-system)
        ("awk" utf-8 . ,locale-coding-system)
        ("curl" utf-8 . ,locale-coding-system)
        ("ffmpeg" utf-8 . ,locale-coding-system)
        (,find-program utf-8 . ,locale-coding-system)
        (,grep-program utf-8 . ,locale-coding-system)
        ("git" utf-8 . ,locale-coding-system)
        ("tectonic" utf-8 . ,locale-coding-system)
        ("ls" utf-8 . ,locale-coding-system)
        ("pandoc" utf-8 . ,locale-coding-system)
        ("make" utf-8 . ,locale-coding-system)
        ("mpv" utf-8 . ,locale-coding-system)
        ("mupdf" utf-8 . ,locale-coding-system)
        ("mutool" utf-8 . ,locale-coding-system)
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


;; jsonrpc
(with-eval-after-load 'gdscript-mode
  (with-eval-after-load 'eglot
    (define-advice jsonrpc--process-filter (:filter-args (args) fix-newline)
      "gdscript eglot."
      (when (string-match-p "\\` \\*EGLOT (.+/.*gdscript-.+) output\\*\\'"
                            (buffer-name (process-buffer (car args))))
        (setcdr args (list (string-replace "\n\n" "\r\n\r\n" (cadr args)))))
      args)))

(provide 'init-winnt)
;;; init-winnt.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mw/" . "my/win-"))
;; End:

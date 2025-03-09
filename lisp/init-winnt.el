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

(defun zw/uwp-visit-localhost (app)
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

(defun zw/display-windows-terminal ()
  "Focus on windows terminal quake window."
  (call-process "wt" nil 0 nil "-w" "_quake" "ft"))

(defconst zw/vanilla-shell shell-file-name)

(defun zw/cmdproxy-real-program-name (cmd)
  "Find invoked real program name in cmd."
  (cl-some (lambda (x) (and (not (string-prefix-p "/" x))
                        (not (string= "start" x)) x))
           (split-string-shell-command
            (replace-regexp-in-string
             (rx (+ space) "\";\"" (* space) eos) "" cmd))))

(defun zw/find-shell-command-coding-system (command)
  "Find coding system for shell command."
  (let ((program (zw/cmdproxy-real-program-name command)))
    (or (find-operation-coding-system 'call-process program)
        default-process-coding-system)))

(define-advice shell-command-on-region (:around (orig-fun &rest args) fix-coding)
  "Fix coding system for `shell-command-on-region' when not in a remote
directory."
  (if (file-remote-p default-directory)
      (apply orig-fun args)
    (let ((process-coding-system-alist
           `(("cmdproxy" ,@(zw/find-shell-command-coding-system (caddr args))))))
      (apply orig-fun args))))

(with-eval-after-load 'ob-eval
  (define-advice org-babel--shell-command-on-region (:around (orig-fun &rest args) fix-coding)
    "Fix coding system for `org-babel--shell-command-on-region' when not in a
remote directory."
    (if (file-remote-p default-directory)
        (apply orig-fun args)
      (let ((process-coding-system-alist
             `(("cmdproxy" ,@(zw/find-shell-command-coding-system (car args))))))
        (apply orig-fun args)))))

(defun zw/proc-coding-system-fix (&optional proc cmd)
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
            (cs (zw/find-shell-command-coding-system
                 (or cmd (nth 2 (process-command proc))))))
      (set-process-coding-system proc (car cs) (cdr cs))
    t))

(dolist (h `(,(derived-mode-hook-name async-shell-command-mode)
             compilation-start-hook))
  (add-hook h #'zw/proc-coding-system-fix))

(defun zw/output-coding-system-fix (cmd output)
  "Fix coding system by convert string."
  (if-let* ((localp (not (file-remote-p default-directory)))
           (cs (zw/find-shell-command-coding-system cmd)))
      (decode-coding-string (encode-coding-string output (cdr cs)) (car cs))
    output))

(defun zw/eshell-change-cs-when-exec (proc)
  "Change the coding system of the Eshell process PROC based on the command
being executed.

It then sets the process coding system for PROC to ensure correct
encoding and decoding of input and output.

The function is intended to be used with `eshell-exec-hook' to
dynamically adjust the coding system for each command executed in
Eshell."
  (when (memq eshell-in-pipeline-p '(nil last))
    (let ((cmd (process-command proc)))
      (zw/proc-coding-system-fix
       proc
       (if (member (car cmd)
                   (list explicit-shell-file-name
                         shell-file-name))
           (nth 2 cmd)
         (car cmd))))))
(add-hook 'eshell-exec-hook #'zw/eshell-change-cs-when-exec)

(defun zw/shell-change-cs-before-send-input (proc string)
  "See `zw/eshell-change-cs-when-exec'."
  (unless (string-empty-p string)
    (zw/proc-coding-system-fix proc string))
  (comint-simple-send proc string))

(defun zw/shell-mode-setup ()
  "Setup for shell-mode."
  (when (string-match-p "cmdproxy"
                        (or explicit-shell-file-name shell-file-name))
    (setq comint-process-echoes t
          comint-input-sender #'zw/shell-change-cs-before-send-input)))
(add-hook 'shell-mode-hook #'zw/shell-mode-setup)

(defun zw/run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\system32\\bash.exe"))
    (shell "*bash*")))

(defun zw/toggle-shell ()
  "Toggle shell between wsl bash and cmd"
  (interactive)
  (setq shell-file-name
        (if (string= shell-file-name zw/vanilla-shell)
            "C:/Windows/system32/bash.exe"
          zw/vanilla-shell)))

(defun zw/coding-conv-region (min from to)
  "Convert coding system between min and `point-max'."
  (unless (file-remote-p default-directory)
    (encode-coding-region min (point-max) from)
    (decode-coding-region min (point-max) to)))

(define-advice epg-encrypt-string (:filter-return (s) remove-\r)
  (string-replace (string ?\r ?\n) (string ?\n) s))


;; sandbox

(cl-defun zw/generate-sandbox-conf
    (&key (gpu "Disable") (net "Disable") map-dirs cmds
          (audio "Disable") (video "Disable") (protect "Enable")
          (printer "Disable") (clip "Disable") (memory "4096"))
  "Generate Windows Sandbox Configuration. ref:
https://learn.microsoft.com/en-us/windows/security/application-security/application-isolation/windows-sandbox/windows-sandbox-configure-using-wsb-file"
  (with-temp-buffer
    (insert-file-contents-literally
     (expand-file-name "sandbox.wsb" auto-insert-directory))
    (let ((param-list
           (list :gpu gpu :net net :audio audio :video video
                 :protect protect :printer printer :clip clip
                 :memory memory :cmds cmds :map-dirs map-dirs))
          (desktop "C:/Users/WDAGUtilityAccount/Desktop")
          (script-dir (expand-file-name (make-temp-name "emacs-tmp-sandbox-cmds-")
                                        temporary-file-directory)))
      (while-let (((re-search-forward (rx "${" (group-n 1 ?: (1+ (not (or ?: ?})))) ?}) nil t))
                  (key (intern (match-string 1))))
        (let ((var (plist-get param-list key)))
          (replace-match
           (pcase key
             ((guard (null var)) "")
             (:cmds
              (cl-letf
                  (((symbol-function 'build-cmd)
                    (lambda (a)
                      (format
                       "<Command>powershell -%s</Command>"
                       (if (stringp a)
                           (concat "E "
                                   (base64-encode-string
                                    (encode-coding-string a 'utf-16le-dos)))
                         (let ((new (concat (make-temp-name "") ".ps1")))
                           (if (plist-member a :script)
                               (write-region (plist-get a :script) nil
                                             (expand-file-name new script-dir))
                             (make-symbolic-link
                              (plist-get a :file)
                              (expand-file-name new script-dir) t))
                           (format "ex Bypsss \"%s/script/%s\"" desktop new)))))))
                (if (stringp var) (build-cmd var)
                  (make-directory script-dir)
                  (push `(:host ,script-dir :box "script") map-dirs)
                  (if (atom (car var)) (build-cmd var)
                    (build-cmd (mapconcat
                                (lambda (a) (replace-regexp-in-string "^<Command>\\(.+\\)</Command>$" "\\1" a))
                                (mapcar #'build-cmd var) ";"))))))
             (:map-dirs
              (cl-letf
                  (((symbol-function 'build-map-dirs)
                    (lambda (a)
                      (apply #'format "<MappedFolder>
  <HostFolder>%s</HostFolder>
  <SandboxFolder>%s</SandboxFolder>
  <ReadOnly>%s</ReadOnly>
</MappedFolder>"
                             (let* ((host (plist-get a :host))
                                    (box (plist-get a :box))
                                    (base (file-name-base (directory-file-name host))))
                               (list
                                (string-replace "/" "\\" host)
                                (string-replace
                                 "/" "\\"
                                 (pcase box
                                   ('nil (expand-file-name base desktop))
                                   ((pred file-name-absolute-p) box)
                                   (_ (expand-file-name box desktop))))
                                (or (plist-get a :ro) "true")))))))
                (if (atom (car map-dirs)) (build-map-dirs map-dirs)
                  (mapconcat #'build-map-dirs map-dirs))))
             (_ var))
           nil t))))
    (buffer-string)))


;; dired

(define-advice insert-directory (:around (orig-fun &rest args) fix-cs)
  "Force decode `ls' output with 'utf-8."
  (condition-case err
      (let ((coding-system-for-read 'utf-8))
        (apply orig-fun args))
    ('file-error
     (let (ls-lisp-use-insert-directory-program)
       (apply orig-fun args)))))

(defun zr-dired-change-onedrive-stat (&optional arg)
  "Toggle OneDrive sync attributes for marked files in dired.

When called without a prefix argument, pins files for offline access by
setting the pinned (+p) attribute and removing the unpinned (-u)
attribute.  With a prefix argument, unpins files by removing the
pinned (-p) attribute and setting the unpinned (+u) attribute.

The function operates recursively on directories (/s) and includes both
files and directories (/d) in the operation.

The command uses the Windows 'attrib' utility and respects the system's
locale encoding for proper handling of non-ASCII filenames."
  (interactive "P")
  (let ((opt '("/s" "/d"))
        (files (dired-get-marked-files))
        (coding-system-for-write locale-coding-system)
        msg)
    (if arg
        (setq opt (append '("-p" "+u") opt)
              msg "free up space")
      (setq opt (append '("+p" "-u") opt)
            msg "always keep on device"))
    (dolist (f files)
      (apply #'call-process "attrib" nil nil nil
             (if (file-directory-p f) (concat f "/*") f) opt))
    (message "%s %s." (mapconcat #'file-name-nondirectory files ", ") msg)))

(with-eval-after-load 'dired
  (dolist (s '(("\\.exe\\'" "innounp -x -o -b -u -pxyg688.com")
               ("\\.ttf\\'" "explorer")))
    (add-to-list 'dired-guess-shell-alist-user s))
  (bind-keys
   :map zr-dired-spc-prefix-map
   ("O" . zr-dired-change-onedrive-stat))
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
  (connection-local-set-profile-variables
   'winnt-fix-coding-system-profile
   '((default-process-coding-system . (utf-8-dos . utf-8-unix))))
  (connection-local-set-profiles
   '(:application tramp) 'winnt-fix-coding-system-profile)
  (setq tramp-default-method "sshx"
        tramp-use-connection-share nil))


;; file

(defun zw/save-with-sudo ()
  "Save the current buffer with sudo permissions."
  (interactive)
  (save-restriction
    (widen)
    (let ((tmp (make-temp-file "emacs-" nil nil (buffer-string))))
      (call-process "sudo" nil nil nil "move"
                    (subst-char-in-string ?/ ?\\ tmp) (buffer-file-name))))
  (set-buffer-modified-p nil))

(let ((exec-path (cl-remove "c:/Windows/system32" exec-path
                            :test #'string-equal-ignore-case)))
  (setq find-program (subst-char-in-string ?/ ?\\ (executable-find "find"))))

(setq grep-program "ug"
      grep-use-null-device nil
      grep-highlight-matches t
      find-ls-option '("-exec ls -ldh \"{}\" \";\" | iconv -f utf-8 -t gb18030 -cs" . "-ldh")
      ls-lisp-use-insert-directory-program t
      default-process-coding-system (cons 'utf-8-dos locale-coding-system)
      ispell-extra-args (list "--filter-path" (substitute-in-file-name "$USERPROFILE/scoop/apps/aspell/current/lib/aspell-0.60"))
      Info-default-directory-list
      (mapcar (lambda (a) (concat (getenv "USERPROFILE") "/scoop/apps/" a))
              '("aspell/current/share/info"
                "mingw-winlibs-llvm-ucrt/current/share/info"))
      process-coding-system-alist
      `(("cmdproxy" . ,locale-coding-system)
        ("ipconfig" . ,locale-coding-system)
        ("findstr" . ,locale-coding-system)
        ("aider" . utf-8))
      file-name-coding-system locale-coding-system
      shr-use-fonts nil)

(setenv "HOME" (file-name-parent-directory user-emacs-directory))

(add-to-list 'exec-suffixes ".ps1")

(dolist (f (directory-files temporary-file-directory t "^emacs-tmp-"))
  (if (file-directory-p f)
      (delete-directory f t))
  (delete-file f))


;; viper

(with-eval-after-load 'viper
  (add-to-list 'zr-viper-extra-ex-token-alist '("ws" (zw/save-with-sudo)))
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


;; mysql
(with-eval-after-load 'sql
  (setq sql-mysql-options '("-tfn" "--compression-algorithms=zstd")))


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
;; read-symbol-shorthands: (("zw/" . "zr-win-"))
;; End:

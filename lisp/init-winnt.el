;;; init-winnt.el --- Personal Windows Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar-local my/scoop-path
    (file-name-concat (getenv "USERPROFILE") "scoop/apps/")
  "scoop apps installation path")

;; prefer
(setq shr-use-fonts nil
      w32-use-native-image-API t
      archive-7z-program "7z")

;; font
(if (eq (display-pixel-width) 1920)
    (set-face-attribute 'default nil :font "LXGW WenKai Mono" :height 108)
  (set-face-attribute 'default nil :font "LXGW WenKai Mono" :height 140))

;; path
(setq find-program "busybox find"
      find-name-arg "-regex"
      grep-use-null-filename-separator nil
      compile-command
      (file-name-concat
       my/scoop-path "msys2/current/usr/bin/make -k ")
      exec-path (append
                 exec-path
                 (list (file-name-concat
                        my/scoop-path "msys2/current/ucrt64/bin")
                       (file-name-concat
                        my/scoop-path "msys2/current/usr/bin")))
      treesit-extra-load-path
      (list (file-name-concat
             my/scoop-path "treesit-langs/current")))
(add-to-list 'exec-suffixes ".ps1")

(defun run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Windows\\system32\\bash.exe"))
    (shell "*bash*")))
(defun my/toggle-shell ()
  "Toggle shell between wsl bash and cmd"
  (interactive)
  (if (string-equal shell-file-name "C:\\Windows\\system32\\bash.exe")
      (setq shell-file-name my/origin-shell)
    (setq my/origin-shell shell-file-name
          shell-file-name "C:\\Windows\\system32\\bash.exe")))


(provide 'init-winnt)
;;; init-winnt.el ends here
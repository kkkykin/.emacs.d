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

(defun ze/pop-output (format)
  "Extract last Eshell output to a new buffer and format it.

FORMAT can be one of the following symbols:
- 'csv : Convert csv to Org table
- 'tsv : Convert TSV to Org table
- 'json : highlight
- 'markdown : Align Markdown table
- nil : No formatting (default)

Interactively, use a prefix argument to select formatting."
  (interactive
   (list (completing-read "Format: " 
                          '("csv" "tsv" "json" "markdown" "plain")
                          nil t))
   eshell-mode)
  (let ((output-buffer (generate-new-buffer "*Eshell Output*"))
        (buf (current-buffer))
        (start (eshell-beginning-of-output))
        (end (eshell-end-of-output)))
    (when (> end start)
      (with-current-buffer output-buffer
        (insert-buffer-substring buf start end)
        (pcase format
          ("csv" (org-table-convert-region (point-min) (point-max) '(4)))
          ("tsv" (org-table-convert-region (point-min) (point-max) '(16)))
          ("json" (json-ts-mode))
          ("markdown" (org-table-align))
          (_ nil)))
      (display-buffer output-buffer))))

(defun ze/sq-cli-handler (&rest args)
  "Eshell command handler for `sq'.

This function determines whether to execute the `sq' command directly
or a wrapper script, depending on whether Eshell is in a
pipeline.

ARGS are passed directly to the `sq` command."
  (let ((rep (if (memq eshell-in-pipeline-p '(first nil))
                 (cons (expand-file-name "run_command.py"
                                         eshell-directory-name)
                       (push "--" args))
               (cons "*sq" (cdr args)))))
    (throw 'eshell-replace-command
           (eshell-parse-command (car rep) (cdr rep)))))

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
The command is executed remotely in the existing Neovim server.

ARGS: A list of arguments to be passed as the command to execute in the
terminal."
  (let ((cmd (format "tabe | tc %s | te %s"
                     (if (file-remote-p default-directory) ""
                       default-directory)
                     (if args (mapconcat #'shell-quote-argument args " ")
                       (let ((last (eshell-get-history 1)))
                         (if (char-equal eshell-explicit-command-char
                                         (aref last 0))
                             (substring last 1)
                           last))))))
    (require 'init-viper)
    (zr-viper-nvim-server-cmd cmd)))

(defun ze/invoke-by-nvim (&rest args)
  "Invoke program in neovim."
  (pcase system-type
    ('windows-nt (zr-win-display-windows-terminal)))
  (throw 'eshell-replace-command
         (eshell-parse-command "vt" args)))

(defvar ze/interpreter-command-alist
  '(("ps1" "powershell" "-NoLogo" "-NoProfile" "-NonInteractive" "-File")
    ("py" "uv" "run" "-s"))
  "Alist mapping file extensions to their interpreter commands.")

(defun ze/invoke-by-shebang (&rest args)
  "Modified from `eshell-script-interpreter'."
  (let ((maxlen eshell-command-interpreter-max-length)
        (file (car args))
        interp)
    (when (and (file-readable-p file)
               (file-regular-p file)
               (> (file-attribute-size (file-attributes file)) 0))
      (with-temp-buffer
        (insert-file-contents-literally file nil 0 maxlen)
        (when (looking-at "#!\\(?:/usr\\)?/bin/env -S \\(.+\\)")
          (when-let* ((cmd (match-string 1)))
            (setq interp (split-string-shell-command cmd))))))
    (unless interp
      (setq interp (alist-get (file-name-extension file)
                              ze/interpreter-command-alist
                              nil nil #'string=)))
    (throw 'eshell-replace-command
	       (eshell-parse-command (car interp) (append (cdr interp) args)))))

(bind-keys
 :map eshell-mode-map
 ("C-c C-v" . ze/pop-output))

(require 'em-term)
(dolist (c '("ssh"
             "usql"))
  (add-to-list 'eshell-visual-commands c))

(when (eq system-type 'windows-nt)
  (delete 'eshell-term eshell-modules-list)
  (dolist (c '(("scoop.cmd" "update" "install")))
    (add-to-list 'eshell-visual-subcommands c))
  (add-to-list 'eshell-interpreter-alist (cons "^sq$" 'ze/sq-cli-handler))
  (add-to-list 'eshell-interpreter-alist
               (cons #'eshell-visual-command-p 'ze/invoke-by-nvim))
  (add-to-list 'eshell-interpreter-alist
               (cons (format "\\.%s\\'"
                             (regexp-opt (mapcar #'car ze/interpreter-command-alist)))
                     'ze/invoke-by-shebang)))

(provide 'init-esh)
;;; init-esh.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ze/" . "zr-eshell-"))
;; End:

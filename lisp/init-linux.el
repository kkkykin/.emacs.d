;;; init-linux.el --- linux setup                    -*- lexical-binding: t; -*-

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

(defun ml/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))

(add-hook 'server-after-make-frame-hook #'ml/setup-faces)

(defun ml/fix-plantuml-args (path args)
  "Fix plantuml Warning: -headless flag must be the first one in the
command line."
  (when-let* ((link (file-symlink-p (executable-find "plantuml"))))
    (with-temp-buffer
      (insert-file-contents link)
      (re-search-forward "^exec .+")
      (let ((cmd (split-string-shell-command (match-string 0))))
        (set path (cadr cmd))
        (set args (append (symbol-value args) (cl-subseq cmd 2 -2)))))))

(with-eval-after-load 'ob-plantuml
  (ml/fix-plantuml-args 'org-plantuml-executable-path 'org-plantuml-args))

(with-eval-after-load 'plantuml-mode
  (ml/fix-plantuml-args 'plantuml-executable-path 'plantuml-executable-args))

(provide 'init-linux)
;;; init-linux.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("ml/" . "my/linux-"))
;; End:

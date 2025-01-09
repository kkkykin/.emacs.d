;;; init-llm.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  kkky

;; Author: kkky <kkkykin@foxmail.com>
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

(defcustom zr-llm-program (executable-find "llama-server")
  "The default llama.cpp path."
  :type 'string)

(defcustom zr-llm-models-directory
  (pcase system-type
    ('windows-nt (substitute-in-file-name "$USERPROFILE/scoop/persist/llama-cpp-cuda/models"))
    (_ nil))
  "The path where store models."
  :type 'directory)

(defcustom zr-llm-default-model
  (and zr-llm-models-directory
       (car (directory-files zr-llm-models-directory t "^[^.]")))
  "The default llm model or path."
  :type 'string)

(defcustom zr-llm-program-args (list "-m" zr-llm-default-model
                                     "--port" "7778")
  "Arguments to `zr-llm-program'."
  :type '(repeat string))

(defcustom zr-llm-process-buf "*llm-server*"
  "llm server buffer name."
  :type 'string)

(defun zr-llm-server-start ()
  "Start llm server."
  (interactive)
  (if (apply #'start-process "*llm-server*" zr-llm-process-buf
             zr-llm-program zr-llm-program-args)
      (message "LLM server started.")
    (message "LLM server start failed.")))

(defun zr-llm-server-stop ()
  "Stop llm server."
  (interactive)
  (kill-buffer zr-llm-process-buf))



(provide 'init-llm)
;;; init-llm.el ends here

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

(defcustom my/llm-program (executable-find "llama-server")
  "The default llama.cpp path."
  :type 'string)

(defcustom my/llm-models-directory
  (pcase system-type
    ('windows-nt (substitute-in-file-name "$USERPROFILE/scoop/persist/llama-cpp-cuda/models"))
    (_ nil))
  "The path where store models."
  :type 'directory)

(defcustom my/llm-default-model
  (and my/llm-models-directory
       (car (directory-files my/llm-models-directory t "^[^.]")))
  "The default llm model or path."
  :type 'string)

(defcustom my/llm-program-args (list "-m" my/llm-default-model
                                     "--port" "7778")
  "Arguments to `my/llm-program'."
  :type '(repeat string))

(defcustom my/llm-process-buf "*llm-server*"
  "llm server buffer name."
  :type 'string)

(defun my/llm-server-start ()
  "Start llm server."
  (interactive)
  (if (apply #'start-process "*llm-server*" my/llm-process-buf
             my/llm-program my/llm-program-args)
      (message "LLM server started.")
    (message "LLM server start failed.")))

(defun my/llm-server-stop ()
  "Stop llm server."
  (interactive)
  (kill-buffer my/llm-process-buf))



(provide 'init-llm)
;;; init-llm.el ends here

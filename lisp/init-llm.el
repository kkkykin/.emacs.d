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

(defcustom zl/program (executable-find "llama-server")
  "The default llama.cpp path."
  :type 'string)

(defcustom zl/models-directory
  (pcase system-type
    ('windows-nt (substitute-in-file-name "$USERPROFILE/scoop/persist/llama-cpp-cuda/models"))
    (_ nil))
  "The path where store models."
  :type 'directory)

(defvar zl/last-model nil
  "The last used llm model.")

(defcustom zl/program-port 7778
  "Port to listen.")

(defcustom zl/program-thread 8
  "Number of threads to use during generation.")

(defcustom zl/program-gpu-layers 36
  "Number of layers to store in VRAM.")

(defcustom zl/program-cache-type-k "q8_0"
  "KV cache data type for K.
Allowed values: f32, f16, bf16, q8_0, q4_0, q4_1, iq4_nl, q5_0, q5_1.")

(defcustom zl/program-args nil
  "Extra arguments to `zl/program'."
  :type '(repeat string))

(defcustom zl/process-buf "*llm-server*"
  "llm server buffer name."
  :type 'string)

(defun zl/read-model ()
  (completing-read "Model: " (directory-files zl/models-directory t "^[^.]")))

(defun zl/server-start (&optional arg)
  "Start llm server."
  (interactive "P")
  (let ((model (if arg (zl/read-model)
                 (or zl/last-model (zl/read-model)))))
    (setq zl/last-model model)
    (if (apply #'start-process "*llm-server*" zl/process-buf
               zl/program "-m" model
               "--port" (number-to-string zl/program-port)
               "-t" (number-to-string zl/program-thread)
               "-ngl" (number-to-string zl/program-gpu-layers)
               "-ctk" zl/program-cache-type-k
               zl/program-args)
        (message "LLM server started.")
      (message "LLM server start failed."))))

(defun zl/server-stop ()
  "Stop llm server."
  (interactive)
  (kill-buffer zl/process-buf))

(provide 'init-llm)
;;; init-llm.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("zl/" . "zr-llm-"))
;; End:

;;; init-org.el --- Org related                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <kkky@KKSBOW>
;; Keywords: extensions

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

;; Org related.

;;; Code:

;; org-protocol

(require 'org-protocol)

(defun my/org-protocol-cookies-dumper (info)
  (let ((parts (org-protocol-parse-parameters info t)))
    (write-region (plist-get parts :cookies) nil
                  (expand-file-name (plist-get parts :host) my/cookies-dir))))

(add-to-list 'org-protocol-protocol-alist
             '("cookies-dumper" :protocol "cookies-dumper"
               :function my/org-protocol-cookies-dumper :kill-client t))

(with-eval-after-load 'org-tempo
  (dolist (k '(("d" . "header")
               ("n" . "name")))
    (add-to-list 'org-tempo-keywords-alist k))

  "ref: `org-tempo-add-keyword'"
  (tempo-define-template
   "org-noweb"
   '( org-babel-noweb-wrap-start
      (completing-read "insert or eval src: " (org-babel-src-block-names))
      p org-babel-noweb-wrap-end)
   "<N"
   "Insert a noweb block."
   'org-tempo-tags)
  (tempo-define-template
   "org-call"
   '("#+call: "
     (completing-read "Call src: " (org-babel-src-block-names))
     "[" p "]()[]")
   "<x"
   "Insert a call keyword"
   'org-tempo-tags))

(defun org-dblock-write:file-finder (params)
  "Create a dynamic block for listing all matched files in a directory and
its subdirectories."
  (let* ((dir (plist-get params :dir))
         (re (plist-get params :re))
         (files (directory-files-recursively dir re))
         (table-header "| Path |\n|------|\n")
         (table-rows (mapconcat (lambda (file) (format "| %s |" file)) files "\n"))
         (table (concat table-header table-rows)))
    (insert table)))

(defun my/org-call-babel-at-point (&optional type params position confirm)
  "Execute the Babel block or call at point in Org mode.

This function executes a Babel source block or a Babel call located at
point, optionally moving to a specified position before executing.

Optional argument TYPE specifies the type of element to execute. If not
provided, the function determines the type based on the element at point.
Supported types are 'src-block', 'inline-src-block', 'babel-call', and
'inline-babel-call'.

Optional argument PARAMS provides additional parameters to pass to the
execution function.

Optional argument POSITION, if non-nil, specifies the position to move to
before executing the Babel block or call.

Optional argument CONFIRM, if non-nil, overrides the value of
`org-confirm-babel-evaluate', determining whether to ask for confirmation
before executing the block.

Example usage:
- Execute a specific Babel call at a given position with confirmation:
  (my/org-call-babel-at-point 'babel-call '((:var . \"a=\\\"ddd\\\"\"))
  1234 t)."
  (save-excursion
    (when position (goto-char position))
    (let ((org-confirm-babel-evaluate confirm))
      (pcase type
        ((or 'src-block 'inline-src-block)
         (org-babel-execute-src-block nil nil params))
        (_
         (when-let* ((ele (org-element-at-point-no-context))
                     (type (org-element-type ele)))
           ;; ref: `org-babel-lob-execute-maybe'
           (org-babel-execute-src-block
            nil (org-babel-lob-get-info ele) params type)))))))

(defun my/org-exec-link-or-babel-nearby (&optional arg)
  "Execute a link or Babel block near the point in Org mode.
Or execute a link near the point in all mode.

This function tries to exec a link or babel at the current point.  If
there is no link at the point, it searches forward for the nearest link
or Babel block and prompts the user to open it or execute it.

When ARG is provided:
- If ARG is '-', search backward.
- If ARG is a number, search that many occurrences forward or backward.
- Otherwise, search forward.

The function supports:
- Org mode links
- Babel calls
- Inline Babel calls
- Inline source blocks
- Source blocks

It handles region selection when searching:
- If a region is active and COUNT is positive, it searches from the end
of the region.
- If COUNT is negative, it searches from the beginning of the region.

If a link is found, the user is prompted to open it.
If a Babel block is found, it is executed.

In case no link or Babel block is found, a user error is signaled."
  (interactive "P")
  (condition-case nil
      (org-open-at-point-global)
    (user-error
     (let ((count (pcase arg
                    ('- -1)
                    ((pred numberp) arg)
                    (_ 1))))
       (if-let ((orgp (eq major-mode 'org-mode))
                (type (car
                       (memq (org-element-type (org-element-at-point))
                             '( babel-call inline-babel-call
                                inline-src-block src-block)))))
           (my/org-call-babel-at-point type)
         (save-excursion
           (re-search-forward
            (if orgp
                (rx (| (regex org-link-any-re)
                       (regex "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)")))
              org-link-any-re)
            (when (use-region-p)
              (if (natnump count) (region-end) (region-beginning)))
            t count)
           (let* ((link (match-string-no-properties 0))
                  (ele (when orgp (org-element-at-point-no-context)))
                  (type (org-element-type ele))
                  (org-link-elisp-confirm-function nil)
                  (org-link-shell-confirm-function nil))
             (cond
              ((string-match-p org-link-any-re link)
               (when (y-or-n-p
                      (format "Open link: %s?"
                              (org-add-props link nil 'face 'org-warning)))
                 (org-link-open-from-string link)))
              ((memq type '( inline-babel-call babel-call
                             inline-src-block src-block))
               (my/org-call-babel-at-point type nil nil t))
              (t (user-error "No link or babel found"))))))))))

(defun my/org-babel-expand-src-block ()
  "Expand the Org Babel source block at point.

This function handles both named source blocks and inline source blocks.
If the point is on a call to a named source block, it navigates to the
named source block, processes its parameters, and then expands it.
Otherwise, it expands the source block at point.

When the point is on a call to a named source block:
- Retrieve the context and the call information.
- Navigate to the named source block.
- Process the parameters of the named source block.
- Expand the source block using `org-babel-expand-src-block`.

If the point is not on a call to a named source block, simply expand the
source block at point.

Usage:
- Place the cursor on a source block or a call to a named source block.
- Call this function interactively to expand the source block.

Example:
Place the cursor on the following line and call `my/org-babel-expand-src-block`:
#+CALL: your-named-src-block()

This will navigate to `your-named-src-block`, process its parameters,
and expand it."
  (interactive)
  (if-let* ((datum (org-element-context))
            (info (org-babel-lob-get-info datum))
            (name (org-element-property :call datum)))
      (save-excursion
        (org-babel-goto-named-src-block name)
        (cl-callf org-babel-process-params (nth 2 info))
        (funcall-interactively #'org-babel-expand-src-block nil info))
    (funcall-interactively #'org-babel-expand-src-block)))

(defun my/org-babel-src-and-call-blocks (&optional file)
  "Return the names of source and call blocks in FILE or the current
buffer. ref: `org-babel-src-block-names'."
  (with-current-buffer (if file (find-file-noselect file) (current-buffer))
    (org-with-point-at 1
      (let ((case-fold-search t)
            blocks)
        (while (re-search-forward org-babel-src-name-regexp nil t)
          (let ((element (org-element-at-point-no-context)))
            (when (memq (org-element-type element)
                        '(src-block babel-call))
              (let ((name (org-element-property :name element)))
                (when name (push (cons name (point)) blocks))))))
        blocks))))

(defun my/org-babel-execute-named-src-block (&optional name params)
  "Execute a named Babel source block or call in the current Org buffer.

This function searches for and executes a named Babel source block or call
within the current Org buffer. If NAME is provided, it directly executes
the block or call with the specified NAME. If NAME is not provided, it prompts
the user to select from available blocks and calls within the buffer.

Optional argument NAME specifies the name of the Babel source block or call
to execute. If not provided, the function will prompt the user to select a name
from the available blocks and calls.

Optional argument PARAMS provides additional parameters to pass to the execution
function. These parameters are merged with a default parameter that sets the
`:results` property to \"silent\" to suppress output.

Usage:
- Execute a specific named Babel block: `M-x my/org-babel-execute-named-src-block`
- Execute a named Babel block with additional parameters:
  (my/org-babel-execute-named-src-block \"block-name\" '((:lexical . \"yes\")))."
  (interactive nil org-mode)
  (save-excursion
    (save-restriction
      (widen)
      (if-let ((params (cons '(:results . "silent") params))
               name)
          (org-with-point-at 1
            (catch 'found
              (let ((case-fold-search t))
                (while (re-search-forward org-babel-src-name-regexp nil t)
                  (when-let* ((element (org-element-at-point-no-context))
                              ((equal name (org-element-property :name element)))
                              (type (org-element-type element))
                              ((memq type '(src-block babel-call))))
                    (throw 'found (my/org-call-babel-at-point type params)))))))
        (if-let* ((blocks (my/org-babel-src-and-call-blocks))
                  (name (completing-read "Src: " (mapcar #'car blocks)))
                  (p (alist-get name blocks nil nil 'equal)))
            (my/org-call-babel-at-point nil params p)
          (user-error "No blocks found."))))))

(with-eval-after-load 'ob
  (bind-keys
   :map org-babel-map
   ([remap org-babel-expand-src-block] . my/org-babel-expand-src-block)
   ("m" . my/org-babel-execute-named-src-block)))

(with-eval-after-load 'ox-latex
  (defun my/org-latex-filter-link-fix (link backend info)
    "Use correct path when export directory not `default-directory'.
     Append zero-width-space after link avoid error: No line here to end."
    (if-let ((need-fix (eq 'latex backend))
             (out-file (plist-get info :output-file)))
        (replace-regexp-in-string
         "\\(\\includegraphics.+{\\)\\(.+\\)}\\([^z-a]+\\)"
         (lambda (s)
           (let ((link (match-string 2 s))
                 (out-dir (file-name-directory
                           (expand-file-name out-file))))
             (string-replace
              "\\" "\\\\"
              (format "%s%s}%s\u200b"
                      (match-string 1 s)
                      (file-relative-name (expand-file-name link)
                                          out-dir)
                      (match-string 3 s)))))
         link)
      link))
  (add-hook 'org-export-filter-link-functions
            #'my/org-latex-filter-link-fix))

(defun my/org-src-save-buffer ()
  "Ask before save org-babel preview buffer."
  (interactive)
  (if (string-prefix-p "*Org-Babel Preview "
                       (buffer-name))
      (when (y-or-n-p "Are you sure want to save expanded buffer?")
        (org-edit-src-save))
    (org-edit-src-save)))

(defun my/viper-save-buffer (&rest args)
  "Save buffer in different modes, customized for Viper and Org interactions.

This function provides a unified interface for saving buffers across
different modes and special buffers in Emacs, particularly when using
Viper mode and Org mode.

It behaves differently based on the current context:
1. In `org-src-mode', it calls `my/org-src-save-buffer'.
2. In the \"*Edit Formulas*\" buffer, it finishes formula editing.
3. In all other cases, it performs a Viper ex-mode write operation."
  (interactive "P")
  (cond
   (org-src-mode (my/org-src-save-buffer))
   ((string= (buffer-name) "*Edit Formulas*")
    (apply #'org-table-fedit-finish args))
   (t (ex-write nil))))

(with-eval-after-load 'org-src
  (add-to-list 'my/extra-ex-token-alist '("w" (my/viper-save-buffer)))
  (bind-keys
   :map org-src-mode-map
   ("C-x C-s" . my/org-src-save-buffer)))

(with-eval-after-load 'viper
  (bind-keys
   :map viper-vi-global-user-map
   ([remap org-open-at-point-global] . my/org-exec-link-or-babel-nearby)))

(provide 'init-org)
;;; init-org.el ends here

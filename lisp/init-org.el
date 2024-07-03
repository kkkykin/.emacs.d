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
                    (_ 1)))
           (orgp (eq major-mode 'org-mode)))
       (if (and orgp
                (org-element-type-p (org-element-context)
                                    '( babel-call inline-babel-call
                                       inline-src-block src-block)))
           (or (org-babel-lob-execute-maybe) (org-babel-execute-src-block))
         (save-excursion
           (re-search-forward
            (if orgp
                (rx (| (regex org-link-any-re)
                       (regex "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)")))
              org-link-any-re)
            (when (use-region-p)
              (if (natnump count) (region-end) (region-beginning)))
            t count)
           (let ((link (match-string-no-properties 0))
                 (ele (when orgp (org-element-context)))
                 (org-link-elisp-confirm-function nil)
                 (org-link-shell-confirm-function nil)
                 (org-confirm-babel-evaluate t))
             (cond
              ((string-match-p org-link-any-re link)
               (when (y-or-n-p
                      (format "Open link: %s?"
                              (org-add-props link nil 'face 'org-warning)))
                 (org-link-open-from-string link)))
              ((org-element-type-p ele '( inline-babel-call babel-call
                                          inline-src-block src-block))
               (or (org-babel-lob-execute-maybe) (org-babel-execute-src-block)))
              (t (user-error "No link or babel found"))))))))))

(with-eval-after-load 'ox-latex
  (defun my/org-latex-filter-link-fix (link backend info)
    "Use correct path when export directory not `default-directory'.
     Append zero-width-space after link avoid error: No line here to end."
    (concat
     (if-let ((need-fix (eq 'latex backend))
              (out-file (plist-get info :output-file)))
         (replace-regexp-in-string
          "\\(\\includegraphics.+{\\)\\(.+\\)}"
          (lambda (s)
            (let ((link (match-string 2 s))
                  (out-dir (file-name-directory
                            (expand-file-name out-file))))
              (string-replace
               "\\" "\\\\"
               (format "%s%s}"
                       (match-string 1 s)
                       (file-relative-name (expand-file-name link)
                                           out-dir)))))
          link)
       link)
     "​"))
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

(with-eval-after-load 'org-src
  (bind-keys
   :map org-src-mode-map
   ("C-x C-s" . my/org-src-save-buffer)))

(defun my/viper-save-buffer (&rest args)
  "Original `ex-write' not support `org-src-mode', use `save-buffer'
instead."
  (interactive "P")
  (if org-src-mode (my/org-src-save-buffer) (ex-write nil)))

(with-eval-after-load 'viper
  (add-to-list 'my/extra-ex-token-alist '("w" (my/viper-save-buffer)))
  (bind-keys
   :map viper-vi-global-user-map
   ([remap org-open-at-point-global] . my/org-exec-link-or-babel-nearby)))

(provide 'init-org)
;;; init-org.el ends here

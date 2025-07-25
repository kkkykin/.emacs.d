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

;; org-keys

;; https://sachachua.com/blog/2025/03/org-mode-cutting-the-current-list-item-including-nested-lists-with-a-speed-command/
(defun zr-org-use-speed-commands ()
  "Activate speed commands on other items too."
  (or (and (looking-at org-outline-regexp) (looking-back "^\**" nil))
      ;; (and (looking-at org-babel-src-block-regexp)
      ;;      (looking-back "^[ \t]*" nil))
      (save-excursion
        (and (looking-at (org-item-re))
             (looking-back "^[ \t]*" nil)))))
(setq org-use-speed-commands 'zr-org-use-speed-commands)

(defun zr-org-cut-subtree (&optional n)
  "Cut current subtree or other."
  (cond
   ((and (looking-at org-outline-regexp) (looking-back "^\**" nil))
    (org-cut-subtree n))
   ;; ((looking-at org-babel-src-block-regexp)
   ;;  (org-babel-remove-result-one-or-many n))
   ((looking-at (org-item-re))
    (kill-region (org-beginning-of-item) (org-end-of-item)))))

(with-eval-after-load 'org-keys
  (dolist (key '(("k" . zr-org-cut-subtree)))
    (setf (alist-get (car key) org-speed-commands nil nil #'string=)
          (cdr key))))

;; org-protocol

(defun zo/protocol-cookies-dumper (info)
  (server-delete-client (car server-clients))
  (let* ((parts (org-protocol-parse-parameters info t))
         (cookie-file (file-name-concat url-configuration-directory
                                        "netscape"
                                        (plist-get parts :host)))
         (buffer-file-coding-system 'utf-8-unix))
    (make-directory (file-name-directory cookie-file) t)
    (write-region (plist-get parts :cookies) nil cookie-file)
    (url-cookie-parse-file-netscape cookie-file))
  (url-cookie-write-file))

(with-eval-after-load 'org-protocol
  (dolist (p '(("cookies-dumper" :protocol "cookies-dumper"
                :function zo/protocol-cookies-dumper :kill-client t)))
    (add-to-list 'org-protocol-protocol-alist p)))

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

(defun zo/string-eval (string)
  "Evaluate STRING as a Lisp expression.
STRING must be a valid Lisp expression in string form.  Returns the
result of evaluating the expression."
  (eval (car (read-from-string string))))

(defun zo/string-maybe-eval (string)
  "Conditionally evaluate STRING as a Lisp expression if it appears to be one.
If STRING matches the pattern of a complete Lisp form (starts with '('
and ends with ')'), evaluate it as a Lisp expression. Otherwise, return
STRING unchanged."
  (if (string-match-p (rx bos ?\( (+ anychar) ?\) eos) string)
      (zo/string-eval string)
    string))

(defun zo/table-select (value-ref tbl key-ref key-value)
  "Select values from a table based on a key.

VALUE-REF(Must contains \"%d\") is the reference to the value in the table TBL.
KEY-REF is the reference to the key in the table TBL.
KEY-VALUE is the value to search for in the key column.

This function resolves the references to the value and key columns,
finds the position of KEY-VALUE in the key column, and returns the
corresponding value from the value column.

For example:
#+name: digit
|   1 | one |
| www | two |

(zo/table-select \"%d,1\" \"digit\" \",0\" 1) => \"one\"
(zo/table-select \"%d,1\" \"digit\" \",0\" \"www\") => \"two\"
"
  (org-babel-ref-resolve
   (format (format "%s[%s]" tbl value-ref)
           (cl-position key-value
                        (org-babel-ref-resolve
                         (format "%s[%s]" tbl key-ref))
                        :test #'equal))))

(defvar org-babel-default-header-args:ahk
  '((:shebang . "#Requires AutoHotkey 2.0+"))
  "Default arguments for evaluating a ahk source block.")

(defvar org-babel-default-header-args:bat
  '((:prologue . "SETLOCAL")
    (:epilogue . "ENDLOCAL"))
  "Default arguments for evaluating a bat source block.")

(defun org-babel-variable-assignments:bat (params)
  "Return list of Bat statements assigning the block's variables.
The variables are defined in PARAMS."
  (mapcar (lambda (pair) (format "set \"%s=%s\"" (car pair) (cdr pair)))
          (org-babel--get-vars params)))

(defun zo/org-remote-link-open (url)
  (let ((auth (car (auth-source-search :host "browse-url.nginx.localhost"
                                       :max 1))))
    (start-process "remote-browse-link" nil "curl"
                   "-H" (concat "origin:ssh://" (system-name))
                   "-H" (concat "url:" url)
                   "-H" (concat "authorization:"
                                (auth-info-password auth))
                   (format "http://127.0.0.1:%s/lua/browse-url"
                           (plist-get auth :port)))))

(defun zo/call-babel-at-point (&optional type params position confirm)
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
  (zo/call-babel-at-point 'babel-call '((:var . \"a=\\\"ddd\\\"\"))
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

(defun zo/execute-link-or-babel-at-point (org-mode-p &optional in-place)
  "Execute the link or Babel element at point."
  (let ((org-link-parameters
         (append
          (when (getenv "SSH_CONNECTION" (selected-frame))
            `(("https" :follow (lambda (s) (zo/org-remote-link-open (concat "https:" s))))
              ("http" :follow (lambda (s) (zo/org-remote-link-open (concat "http:" s))))))
          org-link-parameters)))
    (if in-place
        (condition-case nil
            (org-open-at-point-global)
          (user-error nil))
      (let* ((link (condition-case nil
                       (match-string-no-properties 0)
                     (args-out-of-range "")))
             (element (when org-mode-p (org-element-at-point-no-context)))
             (type (org-element-type element))
             (org-link-elisp-confirm-function nil)
             (org-link-shell-confirm-function nil))
        (cond
         ((string-match-p org-link-any-re link)
          (when (y-or-n-p (format "Open link: %s?" (org-add-props link nil 'face 'org-warning)))
            (org-link-open-from-string link)))
         ((memq type '(inline-babel-call babel-call inline-src-block src-block))
          (zo/call-babel-at-point type))
         (t nil))))))

(defun zo/search-and-execute-link-or-babel (org-mode-p count)
  "Search for and execute a link or Babel element.

Searches forward or backward based on COUNT.  If ORG-MODE-P is true,
prioritizes Org links and Babel elements.  Throws an error if no link
or Babel element is found."
  (let ((regexp (if org-mode-p
                    (rx (| (regex org-link-any-re)
                           (regex "\\(call\\|src\\)_\\|^[ \t]*#\\+\\(BEGIN_SRC\\|CALL:\\)")))
                  org-link-any-re))
        (search-direction (if (natnump count) 'forward 'backward))
        (bound (when (use-region-p)
                 (if (eq search-direction 'forward)
                     (region-end)
                   (region-beginning)))))
    (if (re-search-forward regexp (or bound nil) t count)
        (zo/execute-link-or-babel-at-point org-mode-p)
      (user-error "No link or babel found"))))

(defun zo/exec-link-or-babel-nearby (&optional arg file)
  "Execute link or Babel block/call at point, or nearby.

With prefix arg ARG, search backwards.  With universal arg, start from
beginning of buffer. If on a Babel element, execute it directly.
Otherwise, search for a link or Babel element and execute/open it."
  (interactive "P")
  (with-current-buffer (if file (find-file-noselect file)
                         (current-buffer))
    (let ((count (pcase arg
                   ('- -1)
                   ((pred numberp) arg)
                   (_ 1)))
          (org-mode-p (eq major-mode 'org-mode))
          (in-place t))
      (save-excursion
        (when (equal '(4) arg)
          (goto-char (point-min))
          (setq in-place nil))
        (or (zo/execute-link-or-babel-at-point org-mode-p in-place)
            (zo/search-and-execute-link-or-babel org-mode-p count))))))

(defun zo/babel-expand-src-block ()
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
Place the cursor on the following line and call `zo/babel-expand-src-block`:
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

(defun zo/babel-src-and-call-blocks (&optional file)
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

(defun zo/babel-display-src-block-result (&optional re-run)
  "Display image results of source block at point.

If `point' is on a source block then display the results of the source
code block, otherwise return nil.  With optional prefix argument
RE-RUN the source-code block is evaluated even if results already
exist.
ref: `org-babel-open-src-block-result'"
  (interactive "P")
  (pcase (org-babel-get-src-block-info 'no-eval)
    (`(,_ ,_ ,arguments ,_ ,_ ,start ,_)
     (save-excursion
       ;; Go to the results, if there aren't any then run the block.
       (goto-char start)
       (goto-char (or (and (not re-run) (org-babel-where-is-src-block-result))
		              (progn (org-babel-execute-src-block)
			                 (org-babel-where-is-src-block-result))))
       (end-of-line)
       (skip-chars-forward " \r\t\n")
       ;; Display the results.
       (when (looking-at org-link-bracket-re)
         (org-display-inline-images
          nil t (match-beginning 0) (match-end 0)))))
    (_ nil)))

(defun zo/babel-execute-named-src-block (&optional name params)
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
- Execute a specific named Babel block: `M-x zo/babel-execute-named-src-block`
- Execute a named Babel block with additional parameters:
  (zo/babel-execute-named-src-block \"block-name\" '((:lexical . \"yes\")))."
  (interactive nil org-mode)
  (save-excursion
    (save-restriction
      (widen)
      (if-let* ((params (cons '(:results . "silent") params))
               name)
          (org-with-point-at 1
            (catch 'found
              (let ((case-fold-search t))
                (while (re-search-forward org-babel-src-name-regexp nil t)
                  (when-let* ((element (org-element-at-point-no-context))
                              ((equal name (org-element-property :name element)))
                              (type (org-element-type element))
                              ((memq type '(src-block babel-call))))
                    (throw 'found (zo/call-babel-at-point type params)))))))
        (if-let* ((blocks (zo/babel-src-and-call-blocks))
                  (name (completing-read "Src: " (mapcar #'car blocks)))
                  (p (alist-get name blocks nil nil 'equal)))
            (zo/call-babel-at-point nil params p)
          (user-error "No blocks found."))))))

(defun zo/babel-json-format (name)
  "Run `json-pretty-print-ordered' on json code block."
  (let ((pos (org-babel-find-named-block name)))
    (save-excursion
      (goto-char pos)
      (forward-line)
      (let ((begin (point)))
        (forward-sexp)
        (json-pretty-print-ordered begin (point))))))

(defun zo/completion-at-point ()
  "Provide completion-at-point functionality for source blocks in org-mode.
When point is inside a source block, this function detects the block's
language and calls the corresponding completion function if it
exists. For example, if point is in a Python source block, it will call
`python-completion-at-point'.

The function looks for a completion function named <language>-completion-at-point
where <language> is the source block's language property.

Returns:
- The result of calling the language-specific completion function if found
- nil if point is not in a source block or no completion function exists"
  (let ((ele (org-element-at-point-no-context)))
    (pcase (org-element-type ele)
      ('src-block
       (when-let* ((lan (org-element-property :language ele))
                   (fn (intern (concat lan "-completion-at-point")))
                   ((fboundp fn)))
         (funcall fn)))
      (_ nil))))

(defun zo/hook-setup ()
  "Set up completion-at-point for org-mode buffers.
Adds `zo/completion-at-point' to `completion-at-point-functions'
locally, enabling source block-aware completion in org-mode documents."
  (add-hook 'completion-at-point-functions #'tags-completion-at-point-function nil t)
  (add-hook 'completion-at-point-functions #'zo/completion-at-point nil t))

(with-eval-after-load 'ob
  (add-hook 'org-mode-hook #'zo/hook-setup)
  (bind-keys
   :map org-babel-map
   ("v" . zo/babel-expand-src-block)
   ("q" . zo/babel-display-src-block-result)
   ("m" . zo/babel-execute-named-src-block)))

(defvar zo/babel-confirm-replace-tangle 'ask
  "Confirm before replace by conflicts.")

(defun zo/babel-insert-diff-block (body-start body-end new-body expanded)
  "Insert a diff block in the current buffer.

This function replaces the region between BODY-START and BODY-END with a new
body generated by merging the existing content with NEW-BODY and EXPANDED
content. It uses the `diff3` command to perform the merge.

BODY-START and BODY-END specify the region to be replaced.
NEW-BODY is the new content to be merged.
EXPANDED is the expanded content to be merged.

The function creates temporary files to store the new and expanded content,
calls `diff3` to merge them with the existing content, and then replaces the
specified region with the merged result.

Arguments:
BODY-START -- The start position of the region to be replaced.
BODY-END -- The end position of the region to be replaced.
NEW-BODY -- The new content to be merged.
EXPANDED -- The expanded content to be merged."
  (let ((new-file (make-temp-file "detangle" nil nil new-body))
        (old-file (make-temp-file "detangle" nil nil
                                  (buffer-substring body-start body-end)))
        (expand-file (make-temp-file "detangle" nil nil expanded)))
    (delete-region body-start body-end)
    (forward-line)
    (call-process (executable-find "diff3") nil t nil "-m" expand-file
                  "-Lexpand" old-file "-Lold" new-file "-Lnew")
    (delete-file old-file)
    (delete-file new-file)
    (delete-file expand-file)))

(defun zo/babel-update-block-body (new-body)
  "Update the body of the current code block to NEW-BODY."
  (let ((element (org-element-at-point)))
    (unless (org-element-type-p element 'src-block)
      (error "Not in a source block"))
    (goto-char (org-babel-where-is-src-block-head element))
    (let* ((ind (org-current-text-indentation))
           org-confirm-babel-evaluate
           (body (org-element-normalize-string
		          (if (org-src-preserve-indentation-p element) new-body
		            (with-temp-buffer
		              (insert (org-remove-indentation new-body))
		              (indent-rigidly
		               (point-min)
		               (point-max)
		               (+ ind org-edit-src-content-indentation))
		              (buffer-string)))))
           (expanded (org-babel-expand-src-block)))
      (when (and (not (string= (string-trim body) (string-trim expanded)))
                 (pcase zo/babel-confirm-replace-tangle
                   ('ask (y-or-n-p "Replace by diff-results? "))
                   (_ zo/babel-confirm-replace-tangle)))
        (let ((body-start (line-beginning-position 2))
              (body-end (org-with-wide-buffer
		                 (goto-char (org-element-end element))
		                 (skip-chars-backward " \t\n")
		                 (line-beginning-position))))
          (zo/babel-insert-diff-block
           body-start body-end body (concat expanded "\n"))
          (save-buffer))))))

(defun zo/babel-detangle-1 (&optional source-code-file)
  "Call detangle after bind custom function."
  (cl-letf (((symbol-function 'org-babel-update-block-body)
             #'zo/babel-update-block-body))
    (org-babel-detangle source-code-file)))

(defun zo/babel-detangle (&optional source-code-file)
  "Support detangle single block, call custom babel link, replace by diff
results."
  (interactive "P")
  (pcase current-prefix-arg
    ('nil (zo/babel-detangle-1 source-code-file))
    ('(4)
     (goto-char (pos-bol))
     (or (re-search-forward org-link-bracket-re (pos-eol) t)
         (re-search-backward org-link-bracket-re))
     (let ((begin (pos-bol)))
       (re-search-forward
        (concat " " (regexp-quote (match-string 2)) " ends here") nil t)
       (with-restriction begin (pos-eol) (zo/babel-detangle-1))))
    ((or '(16) (guard force))
     (let ((zo/babel-confirm-replace-tangle
            (y-or-n-p "Always replace conflicts? ")))
       (zo/babel-detangle-1)))
    ('- (org-babel-detangle)))
  (run-at-time 1 nil (lambda () (smerge-start-session t))))

(defvar zo/tangle-default-dir "_tangle"
  "Default directory for tangled code blocks.
When no TANGLE-DIR property is specified in the Org file, code blocks
will be tangled to this directory relative to the Org file's location.")

(defun zo/by-tangle-dir (&optional name no-inherit)
  "Generate absolute file path for tangling the source block at point.

This function determines the tangle destination by combining:
1. The target directory: either from TANGLE-DIR property (inherited or not
inherited) or `zo/tangle-default-dir'.
2. The filename: either from NAME parameter or the source block's NAME property.

ref: https://emacs-china.org/t/header-args-property/27494/2"
  (expand-file-name
   (or name (org-element-property :name (org-element-at-point-no-context)))
   (or (org-entry-get nil "TANGLE-DIR" (not no-inherit)) zo/tangle-default-dir)))

(defun zo/custom-id-get (&optional epom create prefix inherit)
  "ref: `org-id-get'"
  (let ((id (org-entry-get epom "CUSTOM_ID" (and inherit t))))
    (cond
     ((and id (stringp id) (string-match "\\S-" id))
      id)
     (create
      (setq id (org-id-new prefix))
      (org-entry-put epom "CUSTOM_ID" id)
      (org-with-point-at epom
        (org-id-add-location
         id
		 (or org-id-overriding-file-name
			 (buffer-file-name (buffer-base-buffer)))))
      id))))

(with-eval-after-load 'ob-tangle
  (define-advice org-babel-tangle--unbracketed-link
      (:before (params) create-custom-id)
    "If create comments, also create custom-id."
    (unless (string= "no" (cdr (assq :comments params)))
      (zo/custom-id-get nil t))))

(with-eval-after-load 'ox-pandoc
  (defun zo/pandoc-options-fix (body backend info)
    "Fix OPTIONS metadata when export org via pandoc.

This function ensures proper handling of export options when exporting
org-mode using the pandoc backend. It prepends the necessary OPTIONS
metadata to the exported content."
    (if (eq 'pandoc backend)
        (let ((options-string
               (string-join
                (cl-loop for opt in org-export-options-alist
                         for key = (nth 2 opt)
                         for value = (plist-get info (car opt))
                         when key
                         collect (format "%s:%S" key value))
                " ")))
          (format "#+OPTIONS: %s\n%s" options-string
                  (if (memq 'subtree (plist-get info :export-options))
                      body
                    (replace-regexp-in-string "^#\\+options:.+" "" body))))
      body))
  (add-hook 'org-export-filter-final-output-functions
            #'zo/pandoc-options-fix))

(with-eval-after-load 'ox-latex
  (defun zo/latex-filter-link-fix (link backend info)
    "Use correct path when export directory not `default-directory'.
     Append zero-width-space after link avoid error: No line here to end."
    (if (eq 'latex backend)
        (replace-regexp-in-string
         "\\(\\includegraphics.+{\\)\\(.+\\)}\\([^z-a]+\\)"
         (lambda (s)
           (let ((link (match-string 2 s))
                 (out-dir (file-name-directory
                           (expand-file-name (plist-get info :output-file)))))
             (format "%s%s}%s\u200b"
                     (match-string 1 s)
                     (file-relative-name (expand-file-name link)
                                         out-dir)
                     (match-string 3 s))))
         link t t)
      link))
  (add-hook 'org-export-filter-link-functions
            #'zo/latex-filter-link-fix))

(defun zo/src-save-buffer ()
  "Ask before save org-babel preview buffer."
  (interactive)
  (if (string-prefix-p "*Org-Babel Preview "
                       (buffer-name))
      (when (y-or-n-p "Are you sure want to save expanded buffer?")
        (org-edit-src-save))
    (org-edit-src-save)))

(defun zr-viper-save-buffer (&rest args)
  "Save buffer in different modes, customized for Viper and Org interactions.

This function provides a unified interface for saving buffers across
different modes and special buffers in Emacs, particularly when using
Viper mode and Org mode.

It behaves differently based on the current context:
1. In `org-src-mode', it calls `zo/src-save-buffer'.
2. In the \"*Edit Formulas*\" buffer, it finishes formula editing.
3. In all other cases, it performs a Viper ex-mode write operation."
  (interactive "P")
  (cond
   (org-src-mode (zo/src-save-buffer))
   ((string= (buffer-name) "*Edit Formulas*")
    (apply #'org-table-fedit-finish args))
   (t (ex-write nil))))

(with-eval-after-load 'org-src
  (add-to-list 'zr-viper-extra-ex-token-alist '("w" (zr-viper-save-buffer)))
  (add-hook 'org-src-mode-hook #'smerge-start-session)
  (bind-keys
   :map org-src-mode-map
   ("C-x C-s" . zo/src-save-buffer)))

(with-eval-after-load 'viper
  (bind-keys
   :map viper-vi-global-user-map
   ([remap org-open-at-point-global] . zo/exec-link-or-babel-nearby)))

(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("zo/" . "zr-org-"))
;; End:

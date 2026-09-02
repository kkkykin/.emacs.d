;;; init-prog.el --- Prog Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'vc-dir)
(require 'vc-git)
(require 'xref)

(declare-function viper-add-local-keys "viper-keym")
(declare-function viper-change-state-to-insert "viper-cmd")
(declare-function which-function "which-func")
(defvar grep-hit-face)
(defvar compilation-arguments)
(defvar grep-mode-map)
(defvar log-view-mode-map)


;; occur

(defun zp/grep-files ()
  "Extract grep files from current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "default-directory: \"\\(.+\\)\"" (pos-eol) t)
        (let ((default-directory (match-string-no-properties 1))
              (match-face
               (if (and (>= emacs-major-version 30)
                        (bound-and-true-p grep-use-headings))
                   'grep-heading
                 (cons grep-hit-face '(underline))))
              files)
          (while-let ((match (text-property-search-forward
                              'font-lock-face match-face t))
                      (text (buffer-substring-no-properties
                             (prop-match-beginning match)
                             (prop-match-end match)))
                      ((not (string-prefix-p "Grep finished with " text))))
            (push text files))
          (mapcar #'expand-file-name (delete-dups files)))))))

(defun zp/xref-files ()
  "Extract xref files from current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (files)
        (push (buffer-substring-no-properties (point) (pos-eol)) files)
        (while (xref--search-property 'xref-group)
          (push (buffer-substring-no-properties (point) (pos-eol)) files))
        (mapcar #'expand-file-name files)))))

(defun zp/switch-to-occur (&optional nlines)
  "Switch to occur buffer for grep or xref results.
NLINES is the number of context lines to show."
  (interactive "P" grep-mode xref--xref-buffer-mode)
  (when-let* ((files (pcase major-mode
                      ('grep-mode (zp/grep-files))
                      ('xref--xref-buffer-mode (zp/xref-files))))
              ((consp files))
              (re
               (pcase major-mode
                 ('grep-mode
                  (cadr (member "-e" (split-string-shell-command
                                      (car compilation-arguments)))))
                 ('xref--xref-buffer-mode
                  (car-safe (aref (aref xref--fetcher 2) 0)))
                 (_ 'regexp-history-last)))
              (regexp (read-regexp "Regexp" re))
              (nlines (if (natnump nlines) nlines 0))
              (buffers (mapcar (lambda (file)
                                 (or (get-file-buffer file)
                                     (find-file-noselect file t)))
                               files)))
    (occur-1 regexp nlines buffers "*switch-to-occur*")))

(with-eval-after-load 'grep
  (bind-keys
   :map grep-mode-map
   ("C-c C-o" . zp/switch-to-occur)))

(with-eval-after-load 'xref
  (bind-keys
   :map xref--xref-buffer-mode-map
   ("C-c C-o" . zp/switch-to-occur)))


;; xref

(defun zp/next-error-put-function-name-work ()
  "Get function name and put to `next-error-last-buffer'."
  (when-let* ((func (which-function)))
    (set-buffer next-error-last-buffer)
    (let ((text (format "%s │" (string-pad (string-limit func 18) 18)))
          (ov (make-overlay (pos-bol) (1+ (pos-bol)) nil t)))
      (overlay-put ov 'before-string
                   (propertize text 'face 'font-lock-keyward-face))
      (overlay-put ov 'evaporate t)))
  (set-buffer next-error-last-buffer))

(defun zp/next-error-put-function-name (&optional arg)
  "Put function name before all items."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (pcase major-mode
        ('grep-mode
         (widen)
         (goto-char 1)
         (let ((max (- (line-number-at-pos (point-max)) 7)))
           (dotimes (_ max)
             (funcall next-error-function 1)
             (zp/next-error-put-function-name-work))))
        ('occur-mode
         (if arg
             (progn
               (widen)
               (goto-char 1)
               (while (not (condition-case nil
                               (funcall next-error-function 1)
                             (error t)))
                 (zp/next-error-put-function-name-work)))
           (let ((back-cnt 2))
             (or (re-search-forward "^[[:digit:]]+ matches .+ in buffer:" nil t)
                 (and (goto-char (point-max)) (setq back-cnt 1)))
             (let ((max (line-number-at-pos)))
               (re-search-backward "^[[:digit:]]+ matches .+ in buffer:"
                                   nil t back-cnt)
               (dotimes (_ (- max (line-number-at-pos) 1))
                 (funcall next-error-function 1)
                 (zp/next-error-put-function-name-work))))))))))

(bind-keys
 :map occur-mode-map
 ("w" . zp/next-error-put-function-name))

(with-eval-after-load 'grep
  (bind-keys
   :map grep-mode-map
   ("w" . zp/next-error-put-function-name)))

(defun zp/xref-which-function (file pos)
  "Get function name from a marker in a file."
  (with-current-buffer
      (find-file-noselect file)
    (xref--goto-char pos)
    (which-function)))

(defun zp/xref-put-function-name-work ()
  "Put function name before all items."
  (while (not (eobp))
    (forward-line 1)
    (when-let* ((item (xref--item-at-point)))
      (let* ((location (xref-item-location item))
             (file (xref-location-group location))
             (marker (xref-location-marker location))
             (function-name (zp/xref-which-function file marker))
             (ov (make-overlay (pos-bol) (1+ (pos-bol)) nil t))
             (text (format "%s │" (or (string-pad function-name 18) ""))))
        (overlay-put ov 'before-string
                     (propertize text 'face 'font-lock-keyward-face))
        (overlay-put ov 'evaporate t)))))

(defun zp/xref-put-function-name (&optional arg)
  "Put function name before items in current group. If called with
  `universal-argument', apply to the entire buffer."
  (interactive "P")
  (save-excursion
    (if arg
        (progn
          (goto-char (point-min))
          (zp/xref-put-function-name-work))
      (let ((max (or (and (xref--search-property 'xref-group) (point))
                     (point-max))))
        (xref--search-property 'xref-group t)
        (with-restriction (point) max
          (zp/xref-put-function-name-work))))))

(with-eval-after-load 'xref
  (bind-keys
   :map xref--xref-buffer-mode-map
   ("w" . zp/xref-put-function-name)))


;; vc

(with-eval-after-load 'vc-git
  (bind-keys
   :map vc-prefix-map
   :prefix "t"
   :prefix-map zp/vc-git-prefix-map
   :prefix-docstring
   "`vc-git-extra-menu-map', `vc-git-stash-map', `vc-dir-git-mode-map'"
   ("s" . vc-git-stash-snapshot)
   ("c" . vc-git-stash)
   ("d" . vc-git-stash-delete)
   ("v" . vc-git-stash-show)
   ("a" . vc-git-stash-apply)
   ("p" . vc-git-stash-pop)
   ("g" . vc-git-grep)))

(defun zp/vc-dir-copy-filename-as-kill (&optional arg)
  "ref: `dired-copy-filename-as-kill'"
  (interactive "P" vc-dir-mode)
  (let* ((sfiles (or (vc-dir-marked-only-files-and-states)
                     (list (cons (vc-dir-current-file) nil))))
         (files
          (if arg
              (cond ((zerop (prefix-numeric-value arg))
                     (mapcar #'car sfiles))
                    ((consp arg)
                     (mapcar (lambda (f) (file-name-nondirectory (car f))) sfiles))
                    (t (mapcar (lambda (f) (file-relative-name (car f)))
                               sfiles)))
            (mapcar (lambda (f) (file-relative-name (car f))) sfiles)))
         (string
          (if (length= files 1)
              (car files)
            (mapconcat (lambda (file)
                         (if (string-match-p "[ \"']" file)
                             (format "%S" file)
                           file))
                       files
                       " "))))
    (unless (string= string "")
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string))
      (message "%s" string))))

(with-eval-after-load 'vc-dir
  (bind-keys
   :map vc-dir-mode-map
   ("w" . zp/vc-dir-copy-filename-as-kill)
   ("b m" . vc-merge)))

(define-skeleton zp/vc-commit-template
  "VC Conventional Commits.
   https://www.conventionalcommits.org/en/v1.0.0/"
  "Scope: "
  (let ((type (completing-read "Type: "
                               '("fix" "feat" "build" "chore" "ci" "docs"
                                 "style" "refactor" "perf" "test" "merge"
                                 "paste" "empty" "todo"))))
    (pcase type
      ("paste" (current-kill 0))
      ("empty" "")
      ("merge" (with-temp-buffer
                 (format "Merge branch '%s' to '%s'"
                         (vc-read-revision "Merging branch: ")
                         (vc-read-revision "Current branch: "))))
      (_ (format "%s(%s): " type (skeleton-read "Scope: "))))))
(with-eval-after-load 'log-edit
  (add-hook 'log-edit-hook #'zp/vc-commit-template 1))

(defun zp/git-commit-message-setup ()
  "Default insert state, and emulate vc-commit."
  (when (string-suffix-p "/.git/COMMIT_EDITMSG" (buffer-file-name))
    (when (bound-and-true-p viper-mode)
      (viper-change-state-to-insert)
      (viper-add-local-keys
       'insert-state
       '(("\C-c\C-c" . server-edit)
         ("\C-c\C-k" . (lambda () (interactive) (erase-buffer) (server-edit))))))
    (zp/vc-commit-template)))

(add-hook 'server-switch-hook #'zp/git-commit-message-setup)

(with-eval-after-load 'log-view
  (bind-keys
   :map log-view-mode-map
   ("P" . vc-push)))


;; elisp
(defun zp/emacs-mirror-url (&optional blame)
  "Copy the current Emacs source file's GitHub URL.
With prefix argument BLAME, open its blame page."
  (interactive "P")
  (let* ((file (or (buffer-file-name)
                   (user-error "Current buffer has no file")))
         (path (and (string-match
                     "[/\\\\]lisp[/\\\\]\\(.+?\\)\\(?:\\.gz\\)?\\'"
                     file)
                    (match-string 1 file))))
    (unless path
      (user-error "Not an Emacs source file under lisp/"))
    (setq path (subst-char-in-string ?\\ ?/ path))
    (let ((url (format "https://github.com/emacs-mirror/emacs/%s/lisp/%s"
                       (if blame
                           "blame/master"
                         "raw/refs/heads/master")
                       path)))
      (if blame
          (browse-url url)
        (kill-new url)
        (message "Copied: %s" url)))))

(with-eval-after-load 'elisp-mode
  (bind-keys
   :map emacs-lisp-mode-map
   ("C-c RET" . zp/emacs-mirror-url)))


(provide 'init-prog)
;;; init-prog.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("zp/" . "zr-prog-"))
;; End:

;;; init-dir-vc.el --- directory version control        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  

;; Author:  <kkky@KKSBOW>
;; Keywords: files

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

(require 'filenotify)

(defvar zr-dir-vc-watches nil
  "List of active file notification watches (DESCRIPTOR . (DIR FLAGS))")

(defun zr-dir-vc-last-commit-within-seconds-p (seconds &optional dir)
  "Check if last git commit was within SECONDS seconds in DIR."
  (let* ((log-format "%ct")
         (log-cmd (list "git" "-C" (or dir ".") "log" "-1" "--pretty=format:%ct"))
         (last-commit-time (string-to-number (car (apply #'process-lines-ignore-status log-cmd))))
         (current-time (time-to-seconds (current-time))))
    (and (not (zerop last-commit-time))
         (< (- current-time last-commit-time) seconds))))

(defun zr-dir-vc-get-last-commit-message (&optional dir)
  "Get the last git commit message in DIR."
  (car (process-lines-ignore-status
        "git" "-C" (or dir ".") "log" "-1" "--pretty=format:%s")))

(defun zr-dir-vc-status (&optional dir)
  (process-lines "git" "-C" (or dir ".") "status" "-z" "--porcelain"))

(defun zr-dir-vc-commit-all (&optional dir amend)
  (dolist (action `(("add" "-A")
                    ("commit"
                     ,@(and amend '("--amend" "--reset-author"))
                     "-m" ,(format-time-string "%F %T"))))
    (apply #'call-process "git" nil nil nil "-C" (or dir ".") action)))

(defun zr-dir-vc-handle-event (event)
  "Handle file notification EVENT and commit changes with git.
If last commit was within 3 seconds, amend it with accumulated messages
and set commit time to now."
  (let* ((path (if (stringp event) event
                 (let ((desc (car event)))
                   (nth 1 (assoc desc zr-dir-vc-watches)))))
         (dir (if (file-directory-p path) path (file-name-directory path))))
    (unless (string-match-p "/\\.git/?$" (nth 2 event))
      (when (zr-dir-vc-status dir)
        (zr-dir-vc-commit-all
         dir
         (zr-dir-vc-last-commit-within-seconds-p 3 dir))))))

(defun zr-dir-vc (path &optional ignore flags)
  "Start monitoring PATH (file or directory) for file changes matching FLAGS.
PATH: file or directory to monitor
IGNORE: content to write to .gitignore if creating new repo
FLAGS: list of events to monitor (default is '(created deleted renamed changed))

If path is not under git control, initialize it as a git repo (if directory).
If path is under git control, add and commit any untracked or modified files."
  (interactive "fPath to monitor (file or directory): ")
  (setq path (file-truename path))
  (zr-dir-vc-stop path)
  (unless (or (file-directory-p path) (file-exists-p path))
    (error "%s does not exist" path))
  
  (let* ((dir (if (file-directory-p path) path (file-name-directory path)))
         (git-dir (expand-file-name ".git" dir))
         (ignore-file (expand-file-name ".gitignore" dir)))
    (unless (file-directory-p git-dir)
      ;; Initialize new git repo if monitoring a directory
      (when (file-directory-p path)
        (call-process "git" nil nil nil "-C" dir "init")))
    ;; Handle .gitignore
    (unless (file-directory-p path)
      (setq ignore (concat "*\n!" (file-relative-name path dir) "\n")))
    (when (and ignore (file-exists-p ignore-file))
      (unless (y-or-n-p (format ".gitignore already exists in %s. Overwrite? " dir))
        (setq ignore nil)))
    (when ignore
      (write-region ignore nil ignore-file))
    ;; Check for untracked or modified files in existing repo
    (when (and (file-directory-p git-dir) (zr-dir-vc-status dir))
      (zr-dir-vc-commit-all dir)))
  
  (let* ((default-flags '(created deleted renamed changed))
         (watch-flags (or flags default-flags))
         (watch (file-notify-add-watch
                 (if (file-directory-p path) path (file-name-directory path))
                 (cond ((member 'attribute-changed watch-flags)
                        '(change attribute-change))
                       (t '(change)))
                 'zr-dir-vc-handle-event)))
    (push (list watch path watch-flags) zr-dir-vc-watches)
    (message "Started watching file: %s (events: %S)" path watch-flags)))

(defun zr-dir-vc-stop (&optional dir)
  "Stop monitoring DIR. If DIR is nil, stop all monitors."
  (interactive
   (list (when zr-dir-vc-watches
           (completing-read "Directory to stop monitoring (empty for all): "
                            (mapcar #'cadr zr-dir-vc-watches)
                            nil t))))
  (if dir
      (let ((entry (cl-find dir zr-dir-vc-watches :key #'cadr :test #'equal)))
        (when entry
          (file-notify-rm-watch (car entry))
          (setq zr-dir-vc-watches (delq entry zr-dir-vc-watches))
          (message "Stopped watching directory: %s" dir)))
    (dolist (watch zr-dir-vc-watches)
      (ignore-errors
        (file-notify-rm-watch (car watch))))
    (setq zr-dir-vc-watches nil)
    (message "Stopped all directory watches")))

(provide 'init-dir-vc)
;;; init-dirmon.el ends here

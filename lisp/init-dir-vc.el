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
  "List of active file notification watches (DESCRIPTOR . (DIR REGEXP FLAGS))")

(defun zr-dir-vc-matches-p (file regexp)
  "Check if FILE matches REGEXP and is not git directory."
  (and (not (string-match-p "/\\.git/?$" file))
       (or (null regexp) (string-match-p regexp file))))

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

(defun zr-dir-vc-handle-event (event)
  "Handle file notification EVENT and commit changes with git.
If last commit was within 3 seconds, amend it with accumulated messages
and set commit time to now."
  (let* ((desc (nth 0 event))
         (action (nth 1 event))
         (file (nth 2 event))
         (file1 (and (eq action 'renamed) (nth 3 event)))
         (watch-info (assoc desc zr-dir-vc-watches))
         (dir (nth 1 watch-info))
         (regexp (nth 2 watch-info))
         (watch-flags (nth 3 watch-info)))
    
    (when (and dir (member action watch-flags)
               (or (zr-dir-vc-matches-p file regexp)
                   (and file1 (zr-dir-vc-matches-p file1 regexp))))
      (when debug-on-error
        (message "Detected %s on %s" action file))
      
      (when (executable-find "git")
        (let* ((file (file-relative-name file dir))
               (args (pcase action
                       ((or 'created
                            'changed
                            'attribute-changed)
                        (list "add" file))
                       ((or 'deleted
                            'renamed)
                        (list "rm" file))))
               (current-msg (format "%s %s" action file))
               (amend-commit (and (zr-dir-vc-last-commit-within-seconds-p 3 dir)
                                 (not (eq action 'renamed))))
               (last-msg (and amend-commit (zr-dir-vc-get-last-commit-message dir)))
               (msg (cond
                     ((not last-msg) current-msg)
                     ((string-match-p
                       (format "\\(created\\|%s\\) %s$"
                               (regexp-quote action)
                               (regexp-quote file))
                       last-msg)
                      last-msg)
                     (t (concat last-msg "; " current-msg)))))
          
          (apply #'call-process "git" nil nil nil "-C" dir args)
          (when (eq action 'renamed)
            (call-process "git" nil nil nil "-C" dir "add" file1)
            (setq current-msg (concat current-msg " -> " (file-relative-name file1 dir)))
            (setq msg (if amend-commit
                          (concat last-msg "; " current-msg)
                        current-msg)))
          
          (let ((commit-args (if amend-commit
                                 (list "commit" "--amend" "-m" msg "--reset-author")
                               (list "commit" "-m" msg))))
            (unless (zerop (apply #'call-process "git" nil nil nil "-C" dir commit-args))
              (user-error "Dir-VC error: %s %s" dir msg))))))))

(defun zr-dir-vc (dir &optional regexp flags)
  "Start monitoring DIR for file changes matching REGEXP and FLAGS.
DIR: directory to monitor
REGEXP: regexp to match files (nil for all files)
FLAGS: list of events to monitor (default is '(created deleted renamed changed))"
  (interactive "DDirectory to monitor: \nsRegexp to match files (leave empty for all): ")
  (unless (file-directory-p dir)
    (error "%s is not a directory" dir))
  (unless (file-directory-p (expand-file-name ".git" dir))
    (dolist (action '(("init")
                      ("add" ".")
                      ("commit" "-m" "init")))
      (apply #'call-process "git" nil nil nil "-C" dir action)))
  (let* ((default-flags '(created deleted renamed changed))
         (watch-flags (or flags default-flags))
         (watch (file-notify-add-watch
                dir
                (cond ((member 'attribute-changed watch-flags)
                       '(change attribute-change))
                      (t '(change)))
                'zr-dir-vc-handle-event)))
    (push (list watch dir regexp watch-flags) zr-dir-vc-watches)
    (message "Started watching directory: %s%s%s" dir
             (if regexp (format " (matching %s)" regexp) "")
             (format " (events: %S)" watch-flags))))

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

(defun zr-dir-vc-list ()
  "List currently monitored directories."
  (interactive)
  (if zr-dir-vc-watches
      (let ((msg "Currently monitored directories:\n"))
        (dolist (watch zr-dir-vc-watches)
          (let ((dir (nth 1 watch))
                (regexp (nth 2 watch))
                (flags (nth 3 watch)))
            (setq msg (concat msg (format "- %s%s%s\n" dir
                                          (if regexp (format " (matching %s)" regexp) "")
                                          (format " (events: %S)" flags)))))
          (message msg)))
    (message "No directories are currently being monitored")))

(provide 'init-dir-vc)
;;; init-dirmon.el ends here

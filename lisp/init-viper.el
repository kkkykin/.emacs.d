;;; init-viper.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  <kkky@KKSBOW>
;; Keywords: emulations

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

(require 'cl-lib)


;; netrw

(defvar my/dired-target-files nil
  "A list of target directories where marked files can be copied or moved.")

(defun my/dired-get-targets ()
  "Return a list of target files or directories in the current Dired buffer.

If there are marked files in the Dired buffer, return them.
If there are no marked files, return the current directory if in Dired mode,
or the `default-directory` otherwise."
  (or (dired-get-marked-files)
      (list (if (eq 'dired-mode major-mode)
                (dired-current-directory)
              default-directory))))

(defun my/dired-mark-target (&optional path)
  "Mark a directory as a target for copy/move operations in Dired.

If PATH is provided, prompt the user to select a directory to add to the
list of target directories `my/dired-target-files'.
If PATH is not provided, mark the currently selected files or directories
in Dired as target directories.

When called interactively with a prefix argument, always prompt for the
target directory."
  (interactive "P" dired-mode)
  (dolist (f (if path
                 (list (read-directory-name "Target: "))
               (my/dired-get-targets)))
    (add-to-list 'my/dired-target-files f)))

(defun my/dired-unmark-target (&optional target)
  "Unmark a directory from the list of target directories for copy/move
operations in Dired.

If TARGET is provided, prompt the user to select a directory to remove from
the list of target directories `my/dired-target-files'.
If TARGET is not provided, unmark the currently selected files or directories
in Dired from the list of target directories.

When called interactively with a prefix argument, always prompt for the
target to unmark."
  (interactive "P" dired-mode)
  (dolist (f (if target
                 (list (completing-read "Target: " my/dired-target-files))
               (my/dired-get-targets)))
    (setq my/dired-target-files
          (if (string-empty-p f) nil
            (delete f my/dired-target-files)))))

(defun my/dired-dwim-target ()
  "Return a list of target directories for copy/move operations in Dired.

This function combines recent Dired targets from `dired-dwim-target-recent'
with the custom list of target directories `my/dired-target-files',
removing duplicates."
  (cl-delete-duplicates
   (append (dired-dwim-target-recent)
           my/dired-target-files)
   :test #'equal))

(bind-keys
 :map my/dired-spc-prefix-map
 ("t" . my/dired-mark-target)
 ("T" . my/dired-unmark-target))

(setq dired-dwim-target #'my/dired-dwim-target)


;; window

(defun my/buffer-to-side (side &optional buf)
  "Move the current buffer to a window on the specified SIDE.

This function moves the buffer of the currently selected window to a window
at the specified SIDE (left, right, top, or bottom) of the frame. If there
is no window at the specified side, a new window is created by splitting
the frame root window.

The buffer currently in the target window is swapped with the current buffer.

SIDE is a symbol representing the side of the frame where the current buffer
should be moved. It is selected interactively from the options: 'left', 'right',
'top', and 'bottom'."
  (interactive
   (list (intern (completing-read "Side: "
                                  '("left" "right" "top" "bottom")))))
  (if-let* ((fb (or buf (current-buffer)))
            (fw (get-buffer-window fb)))
      (let* ((tws (window-at-side-list nil side))
             (tw (if (< 1 (length tws))
                     (split-window (frame-root-window) nil
                                   (pcase side
                                     ('top 'above)
                                     ('bottom 'below)
                                     (_ side)))
                   (car tws)))
             (tb (window-buffer tw)))
        (set-window-buffer tw fb)
        (if (eq tb fb)
            (delete-window fw)
          (set-window-buffer fw tb))
        (select-window tw))
    (display-buffer-in-direction fb `((direction . ,side) (window . main)))))

(bind-keys
 :map my/viper-cw-prefix-map
 ("H" . (lambda () (interactive) (my/buffer-to-side 'left)))
 ("L" . (lambda () (interactive) (my/buffer-to-side 'right)))
 ("K" . (lambda () (interactive) (my/buffer-to-side 'top)))
 ("J" . (lambda () (interactive) (my/buffer-to-side 'bottom))))

(provide 'init-viper)
;;; init-viper.el ends here

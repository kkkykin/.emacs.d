;;; init-linux.el --- Personal Linux Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun my/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))

(provide 'init-linux)
;;; init-linux.el ends here

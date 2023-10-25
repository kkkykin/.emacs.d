;;; init-linux.el --- Personal Linux Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if (display-graphic-p)
    (set-face-attribute 'default nil :font "LXGW WenKai Mono" :height 108)
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (display-graphic-p frame)
                (with-selected-frame frame
                  (set-face-attribute 'default nil :font "LXGW WenKai Mono" :height 108))))))

(defun my/transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter nil 'alpha-background value))


(provide 'init-linux)
;;; init-linux.el ends here

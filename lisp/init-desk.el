;;; init-desk.el --- Desk Environment Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :hook
  (after-init . (lambda ()
                  (require 'server)
                  (unless (server-running-p)
                    (server-start))))
  :config
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  )

(provide 'init-desk)
;;; init-desk.el ends here

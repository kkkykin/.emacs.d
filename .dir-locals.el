;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((prog-mode . ((tags-table-list . ("~/.emacs.d/TAGS"))))
 ("lisp/init-rss.el.gpg"
  . ((emacs-lisp-mode
      . ((eval . (add-hook 'after-save-hook
                           #'elisp-byte-compile-file nil t)))))))

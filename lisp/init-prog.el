;;; init-prog.el --- Prog Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; treesit

(defun my/ts-mode-enable ()
  "Auto map available ts-mode."
  (dolist (lan (mapcar #'car treesit-language-source-alist))
    (if-let* ((avaip (treesit-language-available-p lan))
              (name (symbol-name lan))
              (fn (intern (format "%s-mode" name)))
              (ts-fn (intern (format "%s-ts-mode"
                                     (pcase name
                                       ("javascript" "js")
                                       (t name)))))
              (ts-fnp (functionp ts-fn))
              (ori-fn (pcase lan
                        (t (or (seq-some
                                (lambda (x) (and (string-match-p (car x) (concat "a." name)) (cdr x)))
                                auto-mode-alist)
                               (and (functionp fn) fn)))))
              (remap (not (eq ori-fn ts-fn))))
        (add-to-list 'major-mode-remap-alist `(,ori-fn . ,ts-fn))
      (when ts-fnp
        (add-to-list 'auto-mode-alist
                     (pcase lan
                       ('yaml `("\\.ya?ml\\'" . ,ts-fn))
                       ('dockerfile `("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . ,ts-fn))
                       (t `(,(format "\\.%s\\'" name) . ,ts-fn))))))))

;; imenu

(defvar my/elisp-imenu-generic-expression
  '(("usep" "^\\s-*(\\use-package \\([[:alpha:]-]+\\)" 1)
    ("tempo" "^\\s-*(\\tempo-define-template\n? *\"\\([[:alpha:]/-]+\\)" 1))
  "Custom imenu generic expression for elisp.")

(defun my/custom-imenu-exp ()
  "Add custom imenu expression for current major-mode."
  (dolist (item (pcase major-mode
                  ('emacs-lisp-mode my/elisp-imenu-generic-expression)))
    (add-to-list 'imenu-generic-expression item)))

(provide 'init-prog)
;;; init-prog.el ends here

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
              (ts-fn (intern (format "%s-ts-mode" name)))
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
                       ('dockerfile `("^Dockerfile\\'" . ,ts-fn))
                       (t `(,(format "\\.%s\\'" name) . ,ts-fn))))))))


(provide 'init-prog)
;;; init-prog.el ends here

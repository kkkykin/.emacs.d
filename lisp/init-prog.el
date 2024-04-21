;;; init-prog.el --- Prog Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; treesit

(defun mp/ts-mode-enable ()
  "Auto map available ts-mode."
  (dolist (lan (mapcar #'car treesit-language-source-alist))
    (if-let* ((avaip (treesit-language-available-p lan))
              (name (symbol-name lan))
              (fn (intern (format "%s-mode" name)))
              (ts-fn (intern (format "%s-ts-mode"
                                     (pcase name
                                       ("javascript" "js")
                                       (_ name)))))
              (ts-fnp (functionp ts-fn))
              (ori-fn (pcase lan
                        (_ (or (seq-some
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
                       (_ `(,(format "\\.%s\\'" name) . ,ts-fn))))))))

(with-eval-after-load 'treesit
  (mp/ts-mode-enable))


;; imenu

(defvar mp/elisp-imenu-generic-expression
  '(("usep" "^\\s-*(\\use-package \\([[:alpha:]-]+\\)" 1)
    ("tempo" "^\\s-*(\\tempo-define-template\n? *\"\\([[:alpha:]/-]+\\)" 1))
  "Custom imenu generic expression for elisp.")

(defun mp/custom-imenu-exp ()
  "Add custom imenu expression for current major-mode."
  (dolist (item (pcase major-mode
                  ('emacs-lisp-mode mp/elisp-imenu-generic-expression)))
    (add-to-list 'imenu-generic-expression item)))
(add-hook 'emacs-lisp-mode-hook 'mp/custom-imenu-exp)


;; sql

(defun mp/sql-fix-imenu-exp ()
  "Fix imenu expression for sql-mode."
  (mapc
   (lambda (a)
     (setcar (cdr a)
             (replace-regexp-in-string "\\\\([^?]" "`?\\&" (cadr a))))
   imenu-generic-expression))
(add-hook 'sql-mode-hook #'mp/sql-fix-imenu-exp)

(defun mp/sql-table-get-pri-key (sqlbuf table)
  "Get primary key name from table."
  (with-temp-buffer
    (sql-execute-feature sqlbuf (current-buffer) :list-table nil table)
    (goto-char (point-min))
    (re-search-forward "^| \\([[:alnum:]_]+\\).+| PRI |")
    (match-string 1)))
  
(defun mp/sql-table-selector (name &optional arg)
  "Select data from NAME. Default select latest 10 records,
with a positive argument, select latest (* 10 number) records;
with a negative argument, select oldest (* 10 number) records;
with `universal argument', select all records."
  (interactive
   (list (sql-read-table-name "Table name: ")
         current-prefix-arg)
   sql-mode sql-interactive-mode)
  (let ((sqlbuf (sql-find-sqli-buffer))
        (builder (list (format "`%s`" name))))
    (unless sqlbuf
      (user-error "No SQL interactive buffer found"))
    (unless name
      (user-error "No table name specified"))
    (when (or (natnump arg) (listp arg))
      (setq builder
            (append builder
                    `("order by"
                      ,(mp/sql-table-get-pri-key sqlbuf name)
                      "desc"))))
    (cond ((and (listp arg)
                (eq nil (car arg)))
           (setq builder
                 (append builder
                         `("limit" "10"))))
          ((numberp arg)
           (setq builder
                 (append builder
                         `("limit" ,(number-to-string (* (abs arg) 10)))))))
    (sql-execute sqlbuf "SQL table selector"
                 "select * from %s;" nil
                 (mapconcat #'identity builder " "))))
  
(tempo-define-template
 "mp/sql-create-procedure"
 '(%""n
    "DROP PROCEDURE IF EXISTS " (P "Procedure name: " procedure) ";"n
    "DELIMITER //"n
    "CREATE PROCEDURE " (s procedure) "("
    (let ((output '(l)))
      (while-let ((dir (completing-read "Direction: " '("IN" "OUT" "INOUT")))
                  (emptyp (not (string= dir ""))))
        (setq output
              (append output
                      (list dir " " (read-no-blanks-input "Variable name: ") " "
                            (upcase (read-no-blanks-input "Data type: ")) ", "))))
      (if (equal output '(l))
          ", "
        output))
    (delete-char -2) ")"n"COMMENT \""p"\""n
    "BEGIN"n n p n n"END//"n"DELIMITER ;"n)
 nil
 "Drop procedure if exists then create it.")

(tempo-define-template
 "mp/sql-create-function"
 '(%""n
    "DROP FUNCTION IF EXISTS " (P "Function name: " function) ";"n
    "DELIMITER //"n
    "CREATE FUNCTION " (s function) "("
    (let ((output '(l)))
      (while-let ((name (read-no-blanks-input "Variable name: "))
                  (emptyp (not (string= name ""))))
        (setq output
              (append output
                      (list name " "
                            (upcase (read-no-blanks-input "Data type: "))
                            ", "))))
      (if (equal output '(l))
          ", "
        output))
    (delete-char -2) ") RETURNS "
    (let* ((return-var (read-no-blanks-input "Retunrs Variable: "
                                             "data_output"))
           (return-type (upcase (read-no-blanks-input "Retunrs Type: ")))
           (return-default (read-string "Returns Default Value: "))
           (output `(l ,return-type n "COMMENT \"" p "\"" n)))
      (while-let ((act (completing-read "Action: "
                                        '("READS SQL DATA"
                                          "MODIFIES SQL DATA"
                                          "NO SQL")))
                  (emptyp (not (string= act ""))))
        (setq output
              (append output
                      `( ,act n "BEGIN" n "DECLARE " ,return-var
                         " " ,return-type
                         ,@(unless (string= "" return-default)
                             (list " DEFAULT " return-default))
                         ";" n n p n n "RETURN " ,return-var ";"))))
      output)
    n"END//"n"DELIMITER ;"n))

(tempo-define-template
 "mp/sql-if"
 '(%"IF " (P "Contidion: ") " THEN"n "  "p n
    (let ((output '(l)))
      (while-let ((elif (read-string "ELSEIF: "))
                  (emptyp (not (string= elif ""))))
        (setq output (append output `("ELSEIF " ,elif " THEN" n "  "p n))))
      (if (y-or-n-p "ELSE: ")
          (append output `("ELSE" n "  "p n))
        output))
    "END IF;"n))

(tempo-define-template
 "mp/sql-case"
 '(%"CASE"n
    (let ((output '(l)))
      (while-let ((co (read-string "Condition: "))
                  (emptyp (not (string= co ""))))
        (setq output (append output `("  WHEN " ,co " THEN" n> "  "p n))))
      (if (y-or-n-p "ELSE: ")
          (append output `("  ELSE" n "  "p n))
        output))
    "END CASE;"n))

(define-skeleton mp/sql-skeleton-while
  "SQL while statement." "Condition: "
  "WHILE " str " DO"\n "  "_ \n "END WHILE;"\n)

(define-skeleton mp/sql-skeleton-repeat
  "SQL repeat statement." "Condition: "
  "REPEAT"\n "  "_ \n"UNTIL " str \n"END REPEAT;"\n)

(define-skeleton mp/sql-skeleton-loop
  "SQL loop statement, use `LEAVE' or `ITERATE' label." "Label: "
  str & ": " "LOOP" \n "  "_ \n "END LOOP " str ";"\n)

(with-eval-after-load 'sql
  (define-keymap :keymap sql-mode-map
    "C-c C-k s" #'mp/sql-table-selector)
  (define-keymap :keymap sql-interactive-mode-map
    "C-c C-k s" #'mp/sql-table-selector)
  (define-abbrev-table 'sql-mode-abbrev-table
    '(("proc" #1="" tempo-template-mp/sql-create-procedure)
      ("fun" #1# tempo-template-mp/sql-create-function)
      ("if" #1# tempo-template-mp/sql-if)
      ("case" #1# tempo-template-mp/sql-case)
      ("while" #1# mp/sql-skeleton-while)
      ("repeat" #1# mp/sql-skeleton-repeat)
      ("loop" #1# mp/sql-skeleton-loop))))


;; sqlite

(defun mp/sqlite-view-file-magically ()
  "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it.
From https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/"
  (let ((file-name buffer-file-name))
    (kill-current-buffer)
    (sqlite-mode-open-file file-name)))

(add-to-list 'magic-mode-alist '("SQLite format 3\x00" . mp/sqlite-view-file-magically))

;; vc

(define-skeleton mp/vc-commit-template
  "VC Conventional Commits.
   https://www.conventionalcommits.org/en/v1.0.0/"
  "Scope: "
  (completing-read "Type: "
                   '("fix" "feat" "build" "chore" "ci" "docs" "style"
                     "refactor" "perf" "test"))
  "("str"): ")
(add-hook 'log-edit-hook #'mp/vc-commit-template 1)


(provide 'init-prog)
;;; init-prog.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("mp/" . "my/prog-"))
;; End:

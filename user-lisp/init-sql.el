;;; init-sql.el --- sql                              -*- lexical-binding: t; -*-

;; Copyright (C) 2026  kkky

;; Author: kkky <kkkykin@foxmail.com>
;; Keywords: convenience

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

(require 'tempo)
(require 'sql)

(declare-function sqlite-mode--column-names "sqlite-mode")
(declare-function url-parse-make-urlobj "url-parse")
(defvar sqlite--db)

(defun zp/sql-connection-to-uri (conn)
  "Convert a SQL connection alist to a URI string.

  Args:
    conn: An alist representing the SQL connection parameters.
      Expected keys are sql-product, sql-user, sql-password,
      sql-server, sql-port, and sql-database.

  Returns:
    A URI string representing the connection, suitable for use with
    libraries like CL-POSTGRES or similar.  Returns nil if the
    conversion fails."
  (let ((urlobj (url-parse-make-urlobj
                 (symbol-name (cadadr (assoc 'sql-product conn)))
                 (car-safe (alist-get 'sql-user conn))
                 (car-safe (alist-get 'sql-password conn))
                 (car-safe (alist-get 'sql-server conn))
                 (car-safe (alist-get 'sql-port conn))
                 (when-let* ((conn (car-safe (alist-get 'sql-database conn))))
                   (concat "/" conn))
                 nil nil t)))
    (url-recreate-url urlobj)))

(defun zp/sqli-buffer-p (buf)
  "Check if BUF is a live SQL interactive buffer.
BUF can be either a buffer object or a string naming a buffer.
Returns non-nil if the buffer exists and is in SQL interactive mode."
  (sql-buffer-live-p (if (stringp buf) (get-buffer buf) (cdr buf))))

(defun zp/read-sqli-buffer (prompt &optional def require-match predicate)
  "ref: `read-buffer'. If PREDICATE is nil, `zp/sqli-buffer-p' is used as
the predicate."
  (let (read-buffer-function)
    (read-buffer prompt def require-match (or predicate #'zp/sqli-buffer-p))))

(defun zp/sql-set-sqli-buffer ()
  "Select a SQL interactive buffer for the current SQL mode buffer.
This is a wrapper around `sql-set-sqli-buffer' that restricts buffer
selection to SQL interactive buffers only."
  (interactive nil sql-mode)
  (let ((read-buffer-function #'zp/read-sqli-buffer))
    (sql-set-sqli-buffer)))

(defun zp/sql-fix-imenu-exp ()
  "Fix imenu expression for sql-mode."
  (mapc
   (lambda (a)
     (setcar (cdr a)
             (replace-regexp-in-string "\\\\([^?]" "`?\\&" (cadr a))))
   imenu-generic-expression))
(add-hook 'sql-mode-hook #'zp/sql-fix-imenu-exp)

(defun zp/sql-table-get-pri-key (sqlbuf table)
  "Get primary key name from table."
  (with-temp-buffer
    (sql-execute-feature sqlbuf (current-buffer) :list-table nil table)
    (goto-char (point-min))
    (re-search-forward "^| \\([[:alnum:]_]+\\).+| PRI |")
    (match-string 1)))

(defun zp/sql-table-selector (name &optional arg)
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
                      ,(zp/sql-table-get-pri-key sqlbuf name)
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
 "zp/sql-create-procedure"
 '(%"DROP PROCEDURE IF EXISTS `" (P "Procedure name: " procedure) "`;"n
    "DELIMITER ;;"n
    "CREATE PROCEDURE `" (s procedure) "`("
    (let ((output '(l)))
      (while-let ((dir (completing-read "Direction: " '("IN" "OUT" "INOUT")))
                  ((not (string-empty-p dir))))
        (setq output
              (append output
                      (list dir " " (read-no-blanks-input "Variable name: ") " "
                            (upcase (read-no-blanks-input "Data type: ")) ", "))))
      (if (equal output '(l))
          ", "
        output))
    (delete-char -2) ")"n"COMMENT '"p"'"n
    "BEGIN"n n p n n"END;;"n"DELIMITER ;"n)
 nil
 "Drop procedure if exists then create it.")

(tempo-define-template
 "zp/sql-create-function"
 '(%"DROP FUNCTION IF EXISTS `" (P "Function name: " function) "`;"n
    "DELIMITER ;;"n
    "CREATE FUNCTION `" (s function) "`("
    (let ((output '(l)))
      (while-let ((name (read-no-blanks-input "Variable name: "))
                  ((not (string-empty-p name)))
                  (type (upcase (read-no-blanks-input "Data type: "))))
        (setq output `(,@output ,(format "%s %s, " name type))))
      (if (equal output '(l)) ", " output))
    (delete-char -2) ") RETURNS "
    (let* ((return-var (read-no-blanks-input "Retunrs Variable: " "_output"))
           (return-type (upcase (read-no-blanks-input "Retunrs Type: ")))
           (return-default (read-string "Returns Default Value: "))
           (actions '("READS SQL DATA" "DETERMINISTIC" "NO SQL"))
           (output `(l ,return-type n "COMMENT '" p "'" n)))
      (while-let ((act (completing-read "Action: " actions))
                  ((not (string-empty-p act))))
        (setq output `(,@output ,act n)))
      `( ,@output "BEGIN" n "DECLARE " ,return-var " " ,return-type
         ,@(unless (string-empty-p return-default)
             (list " DEFAULT " return-default))
         ";" n n p n n "RETURN " ,return-var ";"))
    n"END;;"n"DELIMITER ;"n))

(tempo-define-template
 "zp/sql-create-trigger"
 '(%"DROP TRIGGER IF EXISTS `" (P "Trigger name: " trigger) "`;"n
    "DELIMITER ;;"n
    "CREATE TRIGGER `" (s trigger) "`" n
    (let* ((time (completing-read "Time: " '("BEFORE" "AFTER")))
           (event (completing-read "Event: " '("INSERT" "UPDATE" "DELETE")))
           (tbl (read-no-blanks-input "Table: "))
           (output (list 'l time " " event " ON " tbl " FOR EACH ROW")))
      (if-let* ((order (completing-read "Order: " '("FOLLOW" "PRECEDES")))
                ((not (string-empty-p order)))
                (trig-name (read-no-blanks-input "Trigger: ")))
          (append output (list order " " trig-name))
        output))
    n "main: BEGIN" n n p n n "END main;;" n "DELIMITER ;" n))

(tempo-define-template
 "zp/sql-if"
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
 "zp/sql-case"
 '(%"CASE"n
    (let ((output '(l)))
      (while-let ((co (read-string "Condition: "))
                  (emptyp (not (string= co ""))))
        (setq output (append output `("  WHEN " ,co " THEN" n> "  "p n))))
      (if (y-or-n-p "ELSE: ")
          (append output `("  ELSE" n "  "p n))
        output))
    "END CASE;"n))

(define-skeleton zp/sql-skeleton-while
  "SQL while statement." "Condition: "
  "WHILE " str " DO"\n "  "_ \n "END WHILE;"\n)

(define-skeleton zp/sql-skeleton-repeat
  "SQL repeat statement." "Condition: "
  "REPEAT"\n "  "_ \n"UNTIL " str \n"END REPEAT;"\n)

(define-skeleton zp/sql-skeleton-loop
  "SQL loop statement, use `LEAVE' or `ITERATE' label." "Label: "
  str & ": " "LOOP" \n "  "_ \n "END LOOP " str ";"\n)

(with-eval-after-load 'sql
  (defvar zr-sql-cc-ck-prefix-map
    (make-sparse-keymap))
  (bind-keys
   :map zr-sql-cc-ck-prefix-map
   ("s" . zp/sql-table-selector))
  (define-abbrev-table 'sql-mode-abbrev-table
    '(("proc" #1="" tempo-template-zp/sql-create-procedure)
      ("fun" #1# tempo-template-zp/sql-create-function)
      ("trig" #1# tempo-template-zp/sql-create-trigger)
      ("if" #1# tempo-template-zp/sql-if)
      ("case" #1# tempo-template-zp/sql-case)
      ("while" #1# zp/sql-skeleton-while)
      ("repeat" #1# zp/sql-skeleton-repeat)
      ("loop" #1# zp/sql-skeleton-loop))))


;; sqlite

(defun zp/sqlite-view-file-magically ()
  "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it.
From https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/"
  (let ((file-name buffer-file-name))
    (kill-current-buffer)
    (sqlite-mode-open-file file-name)))

(add-to-list 'magic-mode-alist '("SQLite format 3\x00" . zp/sqlite-view-file-magically))

(provide 'init-sql)
;;; init-sql.el ends here

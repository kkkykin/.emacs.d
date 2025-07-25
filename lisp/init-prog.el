;;; init-prog.el --- Prog Def -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; treesit

(defun zp/ts-mode-enable ()
  "Auto map available ts-mode."
  (interactive)
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
                       ('rust `("\\.rs\\'" . ,ts-fn))
                       ('dockerfile `("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . ,ts-fn))
                       (_ `(,(format "\\.%s\\'" name) . ,ts-fn))))))))

(with-eval-after-load 'treesit
  (zp/ts-mode-enable))


;; occur

(defun zp/grep-files ()
  "Extract grep files from current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "default-directory: \"\\(.+\\)\"" (pos-eol) t)
        (let ((default-directory (match-string-no-properties 1))
              (match-face
               (if (and (>= emacs-major-version 30)
                        (bound-and-true-p grep-use-headings))
                   'grep-heading
                 (cons grep-hit-face '(underline))))
              files)
          (while-let ((match (text-property-search-forward
                              'font-lock-face match-face t))
                      (text (buffer-substring-no-properties
                             (prop-match-beginning match)
                             (prop-match-end match)))
                      ((not (string-prefix-p "Grep finished with " text))))
            (push text files))
          (mapcar #'expand-file-name (delete-dups files)))))))

(defun zp/xref-files ()
  "Extract xref files from current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (files)
        (push (buffer-substring-no-properties (point) (pos-eol)) files)
        (while (xref--search-property 'xref-group)
          (push (buffer-substring-no-properties (point) (pos-eol)) files))
        (mapcar #'expand-file-name files)))))

(defun zp/switch-to-occur (&optional nlines)
  "Switch to occur buffer for grep or xref results.
NLINES is the number of context lines to show."
  (interactive "P" grep-mode xref--xref-buffer-mode)
  (when-let* ((files (pcase major-mode
                      ('grep-mode (zp/grep-files))
                      ('xref--xref-buffer-mode (zp/xref-files))))
              ((consp files))
              (re
               (pcase major-mode
                 ('grep-mode
                  (cadr (member "-e" (split-string-shell-command
                                      (car compilation-arguments)))))
                 ('xref--xref-buffer-mode
                  (car-safe (aref (aref xref--fetcher 2) 0)))
                 (_ 'regexp-history-last)))
              (regexp (read-regexp "Regexp" re))
              (nlines (if (natnump nlines) nlines 0))
              (buffers (mapcar (lambda (file)
                                 (or (get-file-buffer file)
                                     (find-file-noselect file t)))
                               files)))
    (occur-1 regexp nlines buffers "*switch-to-occur*")))

(with-eval-after-load 'grep
  (bind-keys
   :map grep-mode-map
   ("C-c C-o" . zp/switch-to-occur)))

(with-eval-after-load 'xref
  (bind-keys
   :map xref--xref-buffer-mode-map
   ("C-c C-o" . zp/switch-to-occur)))


;; xref

(defun zp/next-error-put-function-name-work ()
  "Get function name and put to `next-error-last-buffer'."
  (when-let* ((func (which-function)))
    (set-buffer next-error-last-buffer)
    (let ((text (format "%s │" (string-pad (string-limit func 18) 18)))
          (ov (make-overlay (pos-bol) (1+ (pos-bol)) nil t)))
      (overlay-put ov 'before-string
                   (propertize text 'face 'font-lock-keyward-face))
      (overlay-put ov 'evaporate t)))
  (set-buffer next-error-last-buffer))

(defun zp/next-error-put-function-name (&optional arg)
  "Put function name before all items."
  (interactive "P")
  (save-window-excursion
    (save-excursion
      (pcase major-mode
        ('grep-mode
         (widen)
         (goto-char 1)
         (let ((max (- (line-number-at-pos (point-max)) 7)))
           (dotimes (i max)
             (funcall next-error-function 1)
             (zp/next-error-put-function-name-work))))
        ('occur-mode
         (if arg
             (progn
               (widen)
               (goto-char 1)
               (while (not (condition-case t
                               (funcall next-error-function 1)
                             (error t)))
                 (zp/next-error-put-function-name-work)))
           (let ((back-cnt 2))
             (or (re-search-forward "^[[:digit:]]+ matches .+ in buffer:" nil t)
                 (and (goto-char (point-max)) (setq back-cnt 1)))
             (let ((max (line-number-at-pos)))
               (re-search-backward "^[[:digit:]]+ matches .+ in buffer:"
                                   nil t back-cnt)
               (dotimes (i (- max (line-number-at-pos) 1))
                 (funcall next-error-function 1)
                 (zp/next-error-put-function-name-work))))))))))

(bind-keys
 :map occur-mode-map
 ("w" . zp/next-error-put-function-name))

(with-eval-after-load 'grep
  (bind-keys
   :map grep-mode-map
   ("w" . zp/next-error-put-function-name)))

(defun zp/xref-which-function (file pos)
  "Get function name from a marker in a file."
  (with-current-buffer
      (find-file-noselect file)
    (xref--goto-char pos)
    (which-function)))

(defun zp/xref-put-function-name-work ()
  "Put function name before all items."
  (while (not (eobp))
    (forward-line 1)
    (when-let* ((item (xref--item-at-point)))
      (let* ((location (xref-item-location item))
             (file (xref-location-group location))
             (marker (xref-location-marker location))
             (function-name (zp/xref-which-function file marker))
             (ov (make-overlay (pos-bol) (1+ (pos-bol)) nil t))
             (text (format "%s │" (or (string-pad function-name 18) ""))))
        (overlay-put ov 'before-string
                     (propertize text 'face 'font-lock-keyward-face))
        (overlay-put ov 'evaporate t)))))

(defun zp/xref-put-function-name (&optional arg)
  "Put function name before items in current group. If called with
  `universal-argument', apply to the entire buffer."
  (interactive "P")
  (save-excursion
    (if arg
        (progn
          (goto-char (point-min))
          (zp/xref-put-function-name-work))
      (let ((max (or (and (xref--search-property 'xref-group) (point))
                     (point-max))))
        (xref--search-property 'xref-group t)
        (with-restriction (point) max
          (zp/xref-put-function-name-work))))))

(with-eval-after-load 'xref
  (bind-keys
   :map xref--xref-buffer-mode-map
   ("w" . zp/xref-put-function-name)))

;; sql

(defun zp/sql-connection-to-uri (conn)
  "Convert a SQL connection alist to a URI string.

  Args:
    conn: An alist representing the SQL connection parameters.
      Expected keys are 'sql-product, 'sql-user, 'sql-password,
      'sql-server, 'sql-port, and 'sql-database.

  Returns:
    A URI string representing the connection, suitable for use with
    libraries like CL-POSTGRES or similar.  Returns nil if the
    conversion fails.

  Example:
    (zp/sql-connection-to-uri
     '((sql-product 'postgresql)))
      (sql-user \"myuser\")
      (sql-password \"mypassword\")
      (sql-server \"localhost\")
      (sql-port 5432)
      (sql-database \"mydb\")))
    => \"postgresql://myuser:mypassword@localhost:5432/mydb\"
  "
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

(with-eval-after-load 'sqlite-mode
  (define-advice sqlite-mode-delete (:override () fix-null)
    "Delete the row under point."
    (interactive nil sqlite-mode)
    (let ((table (get-text-property (point) 'sqlite--type))
          (row (get-text-property (point) 'sqlite--row))
          (inhibit-read-only t))
      (when (or (not (consp table))
                (not (eq (car table) 'row)))
        (user-error "No row under point"))
      (unless (yes-or-no-p "Really delete the row under point? ")
        (user-error "Not deleting"))
      (sqlite-execute
       sqlite--db
       (format "delete from \"%s\" where %s"
               (cdr table)
               (string-join
                (seq-map-indexed
                 (lambda (column i)
                   (format "\"%s\" %s ?"
                           (car (split-string column " "))
                           (if (nth i row) "=" "is")))
                 (cons "rowid" (sqlite-mode--column-names (cdr table))))
                " and "))
       row)
      (delete-region (line-beginning-position) (progn (forward-line 1) (point))))))

(defun zp/sqlite-view-file-magically ()
  "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it.
From https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/"
  (let ((file-name buffer-file-name))
    (kill-current-buffer)
    (sqlite-mode-open-file file-name)))

(add-to-list 'magic-mode-alist '("SQLite format 3\x00" . zp/sqlite-view-file-magically))

;; vc

(with-eval-after-load 'vc-git
  (bind-keys
   :map vc-prefix-map
   :prefix "t"
   :prefix-map zp/vc-git-prefix-map
   :prefix-docstring
   "`vc-git-extra-menu-map', `vc-git-stash-map', `vc-dir-git-mode-map'"
   ("s" . vc-git-stash-snapshot)
   ("c" . vc-git-stash)
   ("d" . vc-git-stash-delete)
   ("v" . vc-git-stash-show)
   ("a" . vc-git-stash-apply)
   ("p" . vc-git-stash-pop)
   ("g" . vc-git-grep)))

(defun zp/vc-dir-copy-filename-as-kill (&optional arg)
  "ref: `dired-copy-filename-as-kill'"
  (interactive "P" vc-dir-mode)
  (let* ((sfiles (or (vc-dir-marked-only-files-and-states)
                     (list (cons (vc-dir-current-file) nil))))
         (files
          (if arg
              (cond ((zerop (prefix-numeric-value arg))
                     (mapcar #'car sfiles))
                    ((consp arg)
                     (mapcar (lambda (f) (file-name-nondirectory (car f))) sfiles))
                    (t (mapcar (lambda (f) (file-relative-name (car f)))
                               sfiles)))
            (mapcar (lambda (f) (file-relative-name (car f))) sfiles)))
         (string
          (if (length= files 1)
              (car files)
            (mapconcat (lambda (file)
                         (if (string-match-p "[ \"']" file)
                             (format "%S" file)
                           file))
                       files
                       " "))))
    (unless (string= string "")
      (if (eq last-command 'kill-region)
          (kill-append string nil)
        (kill-new string))
      (message "%s" string))))

(with-eval-after-load 'vc-dir
  (bind-keys
   :map vc-dir-mode-map
   ("w" . zp/vc-dir-copy-filename-as-kill)
   ("b m" . vc-merge)))

(define-skeleton zp/vc-commit-template
  "VC Conventional Commits.
   https://www.conventionalcommits.org/en/v1.0.0/"
  "Scope: "
  (let ((type (completing-read "Type: "
                               '("fix" "feat" "build" "chore" "ci" "docs"
                                 "style" "refactor" "perf" "test" "merge"
                                 "paste" "empty" "todo"))))
    (pcase type
      ("paste" (current-kill 0))
      ("empty" "")
      ("merge" (with-temp-buffer
                 (format "Merge branch '%s' to '%s'"
                         (vc-read-revision "Merging branch: ")
                         (vc-read-revision "Current branch: "))))
      (_ (format "%s(%s): " type (skeleton-read "Scope: "))))))
(with-eval-after-load 'log-edit
  (add-hook 'log-edit-hook #'zp/vc-commit-template 1))

(defun zp/git-commit-message-setup ()
  "Default insert state, and emulate vc-commit."
  (when (string-suffix-p "/.git/COMMIT_EDITMSG" (buffer-file-name))
    (when (bound-and-true-p viper-mode)
      (viper-change-state-to-insert)
      (viper-add-local-keys
       'insert-state
       '(("\C-c\C-c" . server-edit)
         ("\C-c\C-k" . (lambda () (interactive) (erase-buffer) (server-edit))))))
    (zp/vc-commit-template)))

(add-hook 'server-switch-hook #'zp/git-commit-message-setup)

(with-eval-after-load 'log-view
  (bind-keys
   :map log-view-mode-map
   ("P" . vc-push)))


(provide 'init-prog)
;;; init-prog.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("zp/" . "zr-prog-"))
;; End:

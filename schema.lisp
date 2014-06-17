(in-package :sqlite-obj)

(defun timestamp ()
  "Get unix timestamp."
  (let ((diff 2208988800))  ; (encode-universal-time 0 0 0 1 1 1970 0)
    (- (get-universal-time) diff)))

(defun transform-type-to-sql (type)
  "Given a type (string or keyword) transofrm it into its SQL equivalent."
  (let ((type (if (stringp type)
                  (intern (string-downcase (string type)) :keyword)
                  type)))
    (case type
      (:pkey "nvarchar(32) primary key")
      (:id "nvarchar(32)")
      (:integer "integer")
      (:string "nvarchar(255)")
      ;; 0 = false, 1 = true
      (:bool "tinyint")
      ;; serialize objects to JSON and store as text
      (:object "text")
      (:text "text")
      (:binary "blob"))))

(defun read-schema (db)
  "Talked to Drew about reading the current DB schema. Sounds good."
  (flet ((get-table-info (table-name)
           (sqlite:execute-to-list (dbc db) (format nil "PRAGMA table_info(~a)" table-name)))
         (get-table-indexes (table-name)
           (sqlite:execute-to-list (dbc db) (format nil "PRAGMA index_list(~a)" table-name)))
         (get-db-tables ()
           (sqlite:execute-to-list (dbc db) "SELECT * FROM sqlite_master WHERE type='table'")))
    (let ((info nil))
      (dolist (table (get-db-tables))
        (let ((table-name (cadr table)))
          (push (cons table-name (list :schema (get-table-info table-name)
                                       :indexes (get-table-indexes table-name))) info)))
      (vom:debug2 "read-schema: ~s" info)
      info)))

(defun create-table (db entry &key tmp-table)
  "Create a new table."
  (vom:debug "create-table: ~s" entry)
  (sqlite:execute-non-query
    (dbc db)
    (with-output-to-string (s)
      (format s "CREATE TABLE ~a (" (car entry))
      (dolist (field (append '(("id" . :pkey))
                             (getf (cdr entry) :indexes)
                             `(("data" . ,(if (getf (cdr entry) :binary)
                                              :binary
                                              :text)))))
        (format s "~a ~a~a"
                (car field)
                (transform-type-to-sql (cdr field))
                (if (string= (car field) "data")
                    ""
                    ", ")))
      (format s ")")))
  (dolist (index (getf (cdr entry) :indexes))
    (let ((table (or tmp-table (car entry)))
          (name (car index)))
      (vom:debug "create-table: create-index: ~a_~a" table name)
      (sqlite:execute-non-query
        (dbc db)
        (format nil "CREATE INDEX ~a_~a ON ~a (~a)" table name (car entry) name)))))

(defun delete-table (db name)
  "Delete a table."
  (vom:debug "delete-table: ~a" name)
  (sqlite:execute-non-query (dbc db) (format nil "DROP TABLE IF EXISTS ~a" name)))

(defun rename-table (db from to)
  "Rename a table."
  (vom:debug "rename-table: ~a -> ~a" from to)
  (sqlite:execute-non-query (dbc db) (format nil "ALTER TABLE ~a RENAME TO ~a" from to)))

(defun move-data (db from-table to-table)
  "Migrate data from from-table to to-table (both are string table names)."
  (vom:debug "move-data: ~a -> ~a" from-table to-table)
  (sql-iterate-query (dbc db) (format nil "SELECT * FROM ~a" from-table)
                     (lambda (statement)
                       (db-insert db from-table (statement-to-hash statement) :tmp-table to-table))))

(defun remove-indexes (db table-schema)
  "Delete the indexes from a table."
  (let ((table (car table-schema)))
    (dolist (index (getf (cdr table-schema) :indexes))
      (sqlite:execute-non-query
        (dbc db)
        (format nil "DROP INDEX IF EXISTS ~a_~a" table (car index))))))

(defun merge-table (db from-schema to-schema)
  "Make sure to matches up to from. If needed, we re-create the table and rename
   it to the original (thanks to sqlite having terrible ALTER support)."
  (let* ((standard-fields '("id" "data"))
         (from-fields (remove-if (lambda (x) (find x standard-fields :test 'string=))
                                 (mapcar (lambda (x) (cadr x)) (getf (cdr from-schema) :schema))))
         (from-fields (sort from-fields 'string<))
         (to-fields (sort (mapcar 'car (getf (cdr to-schema) :indexes))
                          'string<)))
    (unless (equalp from-fields to-fields)
      (let ((from-table (car to-schema))
            (to-table (format nil "_tmp_~a" (car to-schema)))
            (copy-schema (copy-tree to-schema)))
        (setf (car copy-schema) to-table)
        (remove-indexes db to-schema)
        (create-table db copy-schema :tmp-table from-table)
        (move-data db from-table to-table)
        (delete-table db from-table)
        (rename-table db to-table from-table)))))

(defun apply-schema (db schema)
  (setf (schema db) schema)
  (let* ((current (read-schema db))
         (current-tables (mapcar 'car current))
         (schema-tables (mapcar 'car schema)))
    ;; add new tables
    (dolist (entry (set-difference schema current :test (lambda (x y) (string= (car x) (car y)))))
      (create-table db entry))
    ;; remove unused tables
    (dolist (entry (set-difference current schema :test (lambda (x y) (string= (car x) (car y)))))
      (delete-table db (car entry)))
    ;; merge existing tables
    (dolist (entry (intersection schema current :test (lambda (x y) (string= (car x) (car y)))))
      (let ((current-entry (find-if (lambda (x) (string= (car x) (car entry)))
                                    current)))
        (merge-table db current-entry entry)))))


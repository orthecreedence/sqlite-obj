(in-package :sqlite-obj)

(define-condition db-missing-schema-entry (db-error) ()
  (:documentation "Thrown when operating on a table that isn't in the current schema."))

(define-condition db-update-missing-id (db-error) ()
  (:documentation "Thrown when an update is triggered on an object without an ID."))

(defun sql-iterate-query (dbc query operate-fn &rest bindings)
  "Given a query, run the operate-fn on each of the results."
  (let ((statement (sqlite:prepare-statement dbc query))
        (results nil))
    (loop for i from 1
          for binding in bindings do
      (sqlite:bind-parameter statement i binding))
    (loop while (sqlite:step-statement statement) do
      (push (funcall operate-fn statement) results))
    (prog1 results
      (sqlite:finalize-statement statement))))

(defun statement-to-hash (statement)
  "Turn a statement result row into a col -> val hash table."
  (let ((columns (sqlite:statement-column-names statement))
        (result (make-hash-table :test #'equal)))
    (loop for i from 0
          for col in columns do
          (setf (gethash col result) (sqlite:statement-column-value statement i)))
    (let ((data-parsed (yason:parse (or (gethash "data" result) "{}"))))
      (loop for key being the hash-keys of data-parsed
            for val being the hash-values of data-parsed do
        (setf (gethash key result) val)))
    (remhash "data" result)
    result))

(defun sql-named-results (db query &rest bindings)
  "Returns the result(s) of the query as a hash table (col -> val). Only works
   with numbered parameters (not named)."
  (let* ((results nil)
         (operate-fn (lambda (statement)
                       (push (statement-to-hash statement) results))))
    (apply 'sql-iterate-query (append (list db query operate-fn)
                                      bindings))
    (nreverse results)))

(defun get-table-schema-entry (schema table)
  "Given a full DB schema, grab the given table's schema entry."
  (find-if (lambda (entry) (string= (car entry) table)) schema))

(defun clone-object (object)
  "Perform a deep clone of an object. Probably not the most efficient way, but
   most certainly the easiest on my fingers."
  (yason:parse
    (with-output-to-string (s)
      (yason:encode object s))))

(defun db-get (table id &key (db *db*))
  "Grab a local-DB object by ID."
  (car (sql-iterate-query (dbc db)
                          (format nil "SELECT * FROM ~a WHERE id = ? LIMIT 1" table)
                          'statement-to-hash
                          id)))

(defun db-insert (table data &key (db *db*) tmp-table ignore)
  "Insert a new object (as hash table) into the local DB."
  (vom:debug1 "db-insert: ~a (~a)" table data)
  (let* ((schema (get-table-schema-entry (schema db) table))
         (indexes (getf (cdr schema) :indexes))
         (binary (getf (cdr schema) :binary-data))
         (dbc (dbc db))
         (copy (clone-object data)))
    (unless schema
      (error 'db-missing-schema-entry :msg (format nil "The table `~a` isn't in the current schema" table)))
    (let* ((sql (with-output-to-string (s)
                  (format s "INSERT ~a INTO ~a (id, "
                          (if ignore
                              "OR IGNORE"
                              "")
                          (or tmp-table table))
                  (dolist (index indexes)
                    (format s "~a, " (car index)))
                  (format s "data) VALUES (")
                  (dotimes (i (+ 1 (length indexes)))
                    (format s "?, "))
                  (format s "?)")))
           (statement (sqlite:prepare-statement dbc sql))
           (bindings nil))
      (push (gethash "id" copy) bindings)
      (sqlite:bind-parameter statement 1 (gethash "id" copy))
      (loop for i from 2
            for (index . nil) in indexes do
        (sqlite:bind-parameter statement i (gethash index copy))
        (push (gethash index copy) bindings)
        (remhash index copy))
      (let ((body (if binary
                      (gethash "data" copy)
                      (with-output-to-string (s)
                        (yason:encode copy s)))))
        (push body bindings)
        (sqlite:bind-parameter statement (+ 2 (length indexes)) body))
      (vom:debug "db-insert: sql: ~a ~s" sql (reverse bindings))
      (sqlite:step-statement statement)
      (sqlite:finalize-statement statement)
      data)))

(defun db-update (table data &key (db *db*))
  "Update a local-DB backed object."
  (vom:debug1 "db-update: ~a (~a)" table data)
  (let* ((schema (get-table-schema-entry (schema db) table))
         (indexes (getf (cdr schema) :indexes))
         (binary (getf (cdr schema) :binary-data))
         (dbc (dbc db))
         (copy (clone-object data)))
    (unless schema
      (error 'db-missing-schema-entry :msg (format nil "The table `~a` isn't in the current schema" table)))
    (unless (gethash "id" data)
      (error 'db-update-missing-id :msg (format nil "Missing `id` field in update data (~a)" table)))
    (let* ((field-values nil)
           (id (gethash "id" copy))
           (sql (with-output-to-string (s)
                  (format s "UPDATE ~a SET " table)
                  (loop for i from 1
                        for (index . nil) in indexes do
                    (let ((exists (nth-value 1 (gethash index copy))))
                      (when exists
                        (format s "~a = ?, " index)
                        (push (gethash index copy) field-values)
                        (remhash index copy))))
                  (format s "data = ? ")
                  (format s "WHERE id = ?")))
           (statement (sqlite:prepare-statement dbc sql)))
      (loop for i from 1
            for val in (reverse field-values) do
        (sqlite:bind-parameter statement i val))
      (let ((body (if binary
                      (gethash "data" copy)
                      (with-output-to-string (s)
                        (yason:encode copy s)))))
        (sqlite:bind-parameter statement (+ 1 (length field-values)) body))
      (sqlite:bind-parameter statement (+ 2 (length field-values)) id)
      (vom:debug "db-update: sql: ~a ~s" sql (append field-values
                                                     (list id :<body>)))
      (sqlite:step-statement statement)
      (sqlite:finalize-statement statement)
      data)))

(defun db-save (table data &key (db *db*))
  "Perform an upsert of the given data."
  (db-insert table data :db db :ignore t)
  (db-update table data :db db))

(defun db-delete (table id &key (db *db*))
  "Delete an object by ID from the local DB."
  (vom:debug1 "db-delete: ~a (~a)" table id)
  (sqlite:execute-non-query
    (dbc db)
    (format nil "DELETE FROM ~a WHERE id = ?" table)
    id))

(defun db-find (table index value &key (db *db*))
  "Find all records matching index -> value."
  (let ((results nil))
    (sql-iterate-query
      (dbc db)
      (format nil "SELECT * FROM ~a WHERE ~a = ?" table index)
      (lambda (statement)
        (push (statement-to-hash statement) results))
      value)
    (nreverse results)))

(defun db-all (table &key (db *db*))
  "Get all records in the table."
  (let ((results nil))
    (sql-iterate-query
      (dbc db)
      (format nil "SELECT * FROM ~a" table)
      (lambda (statement)
        (push (statement-to-hash statement) results)))
    (nreverse results)))


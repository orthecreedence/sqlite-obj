(in-package :sqlite-obj)

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

(defun db-get (db table id)
  "Grab a local-DB object by ID."
  (car (sql-iterate-query (dbc db)
                          (format nil "SELECT * FROM ~a WHERE id = ? LIMIT 1" table)
                          'statement-to-hash
                          id)))

(defun db-insert (db table data &key tmp-table)
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
                  (format s "INSERT INTO ~a (id, " (or tmp-table table))
                  (dolist (index indexes)
                    (format s "~a, " (car index)))
                  (format s "data) VALUES (")
                  (dotimes (i (+ 1 (length indexes)))
                    (format s "?, "))
                  (format s "?)")))
           (statement (sqlite:prepare-statement dbc sql)))
      (sqlite:bind-parameter statement 1 (gethash "id" copy))
      (loop for i from 2
            for (index . nil) in indexes do
        (sqlite:bind-parameter statement i (gethash index copy))
        (remhash index copy))
      (let ((body (if binary
                      (gethash "data" copy)
                      (with-output-to-string (s)
                        (yason:encode copy s)))))
        (sqlite:bind-parameter statement (+ 2 (length indexes)) body))
      (sqlite:step-statement statement)
      (sqlite:finalize-statement statement)
      data)))

(defun db-update (db table data)
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
    (let* ((fields-set nil)
           (sql (with-output-to-string (s)
                  (format s "UPDATE ~a SET " table)
                  (loop for (index . nil) in indexes do
                    (let ((exists (nth-value 1 (gethash index copy))))
                      (remhash index copy)
                      (when exists
                        (format s "~a = ? " index)
                        (push index fields-set))))
                  (format s "data = ? ")
                  (format s "WHERE id = ?")))
           (statement (sqlite:prepare-statement dbc sql)))
      (loop for i from 1
            for field in (reverse fields-set) do
        (sqlite:bind-parameter statement i (gethash field copy)))
      (let ((body (if binary
                      (gethash "data" copy)
                      (with-output-to-string (s)
                        (yason:encode copy s)))))
        (sqlite:bind-parameter statement (+ 1 (length fields-set)) body))
      (sqlite:bind-parameter statement (+ 2 (length fields-set)) (gethash "id" copy))
      (sqlite:step-statement statement)
      (sqlite:finalize-statement statement)
      data)))

(defun db-delete (db table id)
  "Delete an object by ID from the local DB."
  (vom:debug1 "db-delete: ~a (~a)" table id)
  (sqlite:execute-non-query
    (dbc db)
    (format nil "DELETE FROM ~a WHERE id = ?" table)
    id))



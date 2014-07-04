sqlite-obj: A simple document store built on top of sqlite
==========================================================

This project provides an easy means to use SQLite as a document-based storage
system. You tell it the tables you're going to use, the indexes you're going
to search on, and it manages the schema and data transformations for you.

## Documentation

- [database](#database-class)
  - [dbc](#dbc-accessor)
- [\*db\*](#db-variable)
- [db-open](#db-open-function)
- [db-close](#db-close-function)
- [apply-schema](#apply-schema-function)
- [sql-to-objects](#sql-to-objects-function)
- [db-get](#db-get-function)
- [db-save](#db-save-function)
- [db-insert](#db-insert-function)
- [db-update](#db-update-function)
- [db-delete](#db-delete-function)
- [db-find](#db-find-function)
- [db-all](#db-all-function)
- [db-error](#db-error-condition)
- [db-missing-schema-entry](#db-missing-schema-entry-condition)
- [db-update-missing-id](#db-update-missing-id-condition)

### database (class)
Opaque class that holds a database connection and the current schema for that
database. It has one accessor:

#### dbc (acessor)
Get the current `cl-sqlite` database connection for this database. Can be used
to [run direct queries on the database](#sql-to-objects-function).

### \*db\* (variable)
The default database variable. Although you can pass a database to most of the
operations, by default they will use `*db*`.

Usage (see also [db-open](#db-open-function)):

```lisp
(setf *db* (db-open "~/my-db.sqlite"))
```

### db-open (function)
```lisp
(defun db-open (location))
  => database
```
Opens a new database to the location (a filesystem path). Note that `db-open`
does *not* set `*db*`.

### db-close (function)
```lisp
(defun db-close (db))
  => nil
```
Close the given database (the object passed back from [db-open](#db-open-function)).

### apply-schema (function)
```lisp
(defun apply-schema (db schema))
  => nil
```
Apply the given schema to the given database. This function reads the *current*
schema of the database and makes any modifications it needs to to make it
reflect the given schema.

Note that you only need to specify the table name and the indexed fields (the
fields you wish to search on). All other fields are managed automatically.

Example:
```lisp
(let ((db (db-open "~/my-apps-db.sqlite"))
      (schema '(("users"
                 :indexes (("username" . :string)))
                ("actions"
                 :indexes (("user_id" . :integer)
                           ("action" . :string))))))
                 
  (apply-schema db schema))
```

List of index field types (and their translations to SQL):
```lisp
'((:pkey "nvarchar(32) primary key")
  (:id "nvarchar(32)")
  (:integer "integer")
  (:float "float")
  (:string "nvarchar(255)")
  ;; 0 = false, 1 = true
  (:bool "tinyint")
  ;; serialize objects to JSON and store as text
  (:object "text")
  (:text "text")
  (:binary "blob"))
```

Note that sqlite-obj assumes 32-character strings as PKEYs/IDs. This is the
requirement if the app it was built for. Most people will probably need numeric
PKEYs, so just let me know and I'll figure out a way to appease everybody.

### sql-to-objects (function)
```lisp
(defun sql-to-objects (dbc query &rest bindings))
  => list-of-hashes
```
Takes a database connection (retrieved with [dbc](#dbc-accessor)) and an SQL
query along with a set of bindings, and returns a list of objects.

Example:
```lisp
(sql-to-objects (dbc my-db)
                "SELECT * FROM users WHERE age < ?"
                32)
  => (<user hash> <user hash> ...)
```

This allows running direct queries on your data so you're not restricted to the
obnoxious primitives you get below (which are convenience but limiting).


### db-get (function)
```lisp
(defun db-get (table id &key (db *db*)))
  => hash/nil
```
Get an object from `table` by `id`.

### db-save (function)
```lisp
(defun db-save (table data &key (db *db*)))
  => data
```
Save an item in the database. If it has an ID and already exists, it will be
updated, otherwise inserted. `data` is a hash table of the data to save.

### db-insert (function)
```lisp
(defun db-insert (table data &key (db *db*) tmp-table ignore))
  => data
```
Isert `data` (hash table) into `table`. Note that your inserted `data` must have
an "id" field.

`:tmp-table` is only used by [apply-schema](#apply-schema-function) internally.
`:ignore` tells `insert` to *not* perform an insert if the `data` being inserted
has an ID that already exists in the table.

### db-update (function)
```lisp
(defun db-update (table data &key (db *db*)))
  => data
```
Update `data` in `table`. Note that the "id" field must be present, otherwise
`db-update` will throw a [db-update-missing-id error](#db-update-missing-id-condition).

### db-delete (function)
```lisp
(defun db-delete (table id &key (db *db*)))
  => id
```
Delete the object with the id `id` from `table`.

### db-find (function)
```lisp
(defun db-find (table index value &key (db *db*)))
  => results
```
Find all records in `table` where `index` is equal to `value`. `index` must be a
field specified in the `:indexes` portion of the table's schema.

### db-all (function)
```lisp
(defun db-all (table &key (db *db*)))
  => results
```
Return all records from `table`.

### db-error (condition)
A general error, extended by all other errors.

### db-missing-schema-entry (condition)
Thrown when a reference is made to a field that doesn't exist in the current
table's schema.

### db-update-missing-id (condition)
Thrown when an object is passed to [db-insert](#db-insert-function) /
[db-update](#db-update-function) that doesn't have an "id" field.

## Tests

```lisp
(asdf:operate 'asdf:load-op :sqlite-obj-test)
(sqlite-obj-test:run-tests)
```

## License

MIT.


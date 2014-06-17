(defpackage :sqlite-obj-test
  (:use :cl :fiveam :sqlite-obj)
  (:shadowing-import-from :sqlite-obj
                          :get)
  (:import-from :sqlite-obj
                :database)
  (:export :run-tests))
(in-package :sqlite-obj-test)

(defparameter *root* (asdf:system-relative-pathname :sqlite-obj #P"test/"))

(def-suite sqlite-obj :description "main sqlite-obj test suite")
(in-suite sqlite-obj)

(defun test-db-name (name)
  "Give a full path to a test DB."
  (format nil "~a/~a.sqlite" *root* name))

(def-fixture db-conn ()
  (let ((db (db-open (test-db-name "test-db"))))
    (unwind-protect
      (let ((schema '(("users")
                      ("notes"
                       :indexes (("user_id" . :id)
                                 ("board_id" . :id)
                                 ("has_file" . :bool))))))
        (apply-schema db schema)
        (&body))
      (db-close db))))

(test db-open-close
  "Test DBs open."
  (let* ((path (test-db-name "slappy"))
         (db (db-open path)))
    (is (probe-file path))
    (is (typep db 'database))
    (db-close db)
    (is (probe-file path))
    (delete-file path)))

(test apply-schema
  "Make sure we can apply schemas, and also migrate data effectively."
  (with-fixture db-conn ()
    (let ((schema2 '(("users")
                     ("notes"
                      :indexes (("user_id" . :id)
                                ("has_file" . :bool))))))
      (db-insert db "notes"
                 (let ((hash (make-hash-table :test #'equal)))
                   (setf (gethash "id" hash) "1234")
                   (setf (gethash "user_id" hash) "6969")
                   (setf (gethash "board_id" hash) "9999")
                   (setf (gethash "text" hash) "omg lol wtf")
                   hash))
      (is (equalp '(("users"
                     :SCHEMA
                      ((0 "id" "nvarchar(32)" 0 NIL 1)
                       (1 "data" "text" 0 NIL 0))
                     :INDEXES
                      ((0 "sqlite_autoindex_users_1" 1)))
                     ("notes"
                      :SCHEMA
                       ((0 "id" "nvarchar(32)" 0 NIL 1)
                        (1 "user_id" "nvarchar(32)" 0 NIL 0)
                        (2 "board_id" "nvarchar(32)" 0 NIL 0)
                        (3 "has_file" "tinyint" 0 NIL 0)
                        (4 "data" "text" 0 NIL 0))
                       :INDEXES
                        ((0 "notes_has_file" 0)
                         (1 "notes_board_id" 0)
                         (2 "notes_user_id" 0)
                         (3 "sqlite_autoindex_notes_1" 1))))
                  (sqlite-obj::read-schema db)))
      (let ((note-record (car (sqlite:execute-to-list (dbc db) "SELECT * FROM notes"))))
        (is (equalp '("1234" "6969" "9999" NIL "{\"text\":\"omg lol wtf\",\"id\":\"1234\"}")
                    note-record)))
      (apply-schema db schema2)
      (is (equalp '(("notes"
                      :SCHEMA
                       ((0 "id" "nvarchar(32)" 0 NIL 1)
                        (1 "user_id" "nvarchar(32)" 0 NIL 0)
                        (2 "has_file" "tinyint" 0 NIL 0)
                        (3 "data" "text" 0 NIL 0))
                       :INDEXES
                        ((0 "notes_has_file" 0)
                         (1 "notes_user_id" 0)
                         (2 "sqlite_autoindex_notes_1" 1)))
                    ("users"
                     :SCHEMA
                      ((0 "id" "nvarchar(32)" 0 NIL 1)
                       (1 "data" "text" 0 NIL 0))
                     :INDEXES
                      ((0 "sqlite_autoindex_users_1" 1))))
                  (sqlite-obj::read-schema db)))
      (let ((note-record (car (sqlite:execute-to-list (dbc db) "SELECT * FROM notes"))))
        (is (equalp '("1234" "6969" NIL "{\"text\":\"omg lol wtf\",\"board_id\":\"9999\",\"id\":\"1234\"}")
                    note-record))))))

(test (db-insert :depends-on apply-schema)
  "Test inthertion"
  (with-fixture db-conn ()
    (db-insert db "users"
               (let ((hash (make-hash-table :test #'equal)))
                 (setf (gethash "id" hash) "1234")
                 (setf (gethash "name" hash) "Dartanion")
                 hash))
    (db-insert db "users"
               (let ((hash (make-hash-table :test #'equal)))
                 (setf (gethash "id" hash) "666")
                 (setf (gethash "name" hash) "Dario")
                 hash))
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 2 num)))))

(test (db-get :depends-on db-insert)
  (with-fixture db-conn ()
    (let ((rec1 (db-get db "users" "1234"))
          (rec2 (db-get db "users" "666")))
      (is (string= "Dartanion" (gethash "name" rec1)))
      (is (string= "Dario" (gethash "name" rec2))))))

(test (db-update :depends-on db-get)
  (with-fixture db-conn ()
    (db-update db "users" (let ((hash (make-hash-table :test #'equal)))
                            (setf (gethash "id" hash) "666")
                            (setf (gethash "name" hash) "Sex Robot")
                            hash))
    (let ((rec (db-get db "users" "666")))
      (is (string= "Sex Robot" (gethash "name" rec))))))

(test (db-delete :depends-on db-update)
  (with-fixture db-conn ()
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 2 num)))
    (db-delete db "users" "666")
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 1 num)))
    (db-delete db "users" "1234")
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 0 num)))))

(defun run-tests ()
  (let ((test-db (test-db-name "test-db")))
    (when (probe-file test-db)
      (delete-file test-db)))
  (run! 'sqlite-obj))


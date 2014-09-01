(defpackage :sqlite-obj-test
  (:use :cl :fiveam :sqlite-obj)
  (:shadowing-import-from :sqlite-obj
                          :get)
  (:import-from :sqlite-obj
                :database
                :dbc
                :schema)
  (:export :run-tests))
(in-package :sqlite-obj-test)

(defparameter *root* (asdf:system-relative-pathname :sqlite-obj #P"test/"))

;(vom:config :sqlite-obj :debug)

(def-suite sqlite-obj :description "main sqlite-obj test suite")
(in-suite sqlite-obj)

(defun test-db-name (name)
  "Give a full path to a test DB."
  (format nil "~a/~a.sqlite" *root* name))

(defun jprint (db-result &key (stream *standard-output*))
  "Pretty printer for JSON (mainly for database results)."
  (yason:encode db-result (yason:make-json-output-stream stream :indent 2)))

(def-fixture db-conn ()
  (let ((db (db-open (test-db-name "test-db"))))
    (setf *db* db)
    (unwind-protect
      (let ((schema '(("users")
                      ("notes"
                       :indexes (("user_id" . :id)
                                 ("board_id" . :id)
                                 ("has_file" . :bool))))))
        (apply-schema db schema)
        (&body))
      (db-close db)
      (setf *db* nil))))

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
      (db-insert "notes"
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
    (db-insert "users"
               (let ((hash (make-hash-table :test #'equal)))
                 (setf (gethash "id" hash) "1234")
                 (setf (gethash "name" hash) "Dartanion")
                 hash))
    (db-insert "users"
               (let ((hash (make-hash-table :test #'equal)))
                 (setf (gethash "id" hash) "666")
                 (setf (gethash "name" hash) "Dario")
                 (setf (gethash "says" hash) "Oh yeah? Did she tell you about THESE?!?!?!")
                 hash))
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 2 num)))))

(test (db-get :depends-on db-insert)
  (with-fixture db-conn ()
    (let ((rec1 (db-get "users" "1234"))
          (rec2 (db-get "users" "666")))
      (is (string= "Dartanion" (gethash "name" rec1)))
      (is (string= "Dario" (gethash "name" rec2))))))

(test (db-update :depends-on db-get)
  (with-fixture db-conn ()
    (db-update "users" (let ((hash (make-hash-table :test #'equal)))
                         (setf (gethash "id" hash) "666")
                         (setf (gethash "name" hash) "Sex Robot")
                         (setf (gethash "job" hash) "shoveling poop")
                         hash))
    (let ((rec (db-get "users" "666")))
      (is (string= "Sex Robot" (gethash "name" rec))))))

(test (db-find :depends-on db-update)
  (with-fixture db-conn ()
    (let ((items (db-find "notes" "user_id" "6969")))
      (is (= (length items) 1))
      (is (string= "1234" (gethash "id" (car items)))))))

(test (db-save :depends-on db-find)
  (with-fixture db-conn ()
    (let* ((note-data (let ((hash (make-hash-table :test #'equal)))
                        (setf (gethash "id" hash) "1226")
                        (setf (gethash "user_id" hash) "6969")
                        (setf (gethash "board_id" hash) "8888")
                        (setf (gethash "text" hash) "OMG are those DIOR?!?!")
                        hash)))
      (db-save "notes" note-data)
      (let ((note (db-get "notes" "1226")))
        (is (string= "8888" (gethash "board_id" note))))
      (setf (gethash "board_id" note-data) "4444")
      (db-save "notes" note-data)
      (let ((note (db-get "notes" "1226")))
        (is (string= "4444" (gethash "board_id" note)))))))

(test (db-all :depends-on db-save)
  (with-fixture db-conn ()
    (let ((notes (db-all "notes"))
          (users (db-all "users")))
      (is (= 2 (length notes)))
      (is (= 2 (length users))))))

(test (db-delete :depends-on db-all)
  (with-fixture db-conn ()
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 2 num)))
    (db-delete "users" "666")
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 1 num)))
    (db-delete "users" "1234")
    (let ((num (sqlite:execute-single (dbc db) "SELECT COUNT(*) FROM users")))
      (is (= 0 num)))))

(test (autoinc :depends-on db-delete)
  (with-fixture db-conn ()
    (let ((schema '(("queue"
                     :id :rowid
                     :indexes (("grabbed" . :integer)))))
          (job (make-hash-table :test 'equal)))
      (setf (gethash "task" job) "get a job")
      (apply-schema db schema)
      (vom:config :sqlite-obj :debug)
      (let* ((job (db-save "queue" job))
             (jobs (db-all "queue")))
        (vom:config :sqlite-obj :warn)
        (format t "jobs: ~a~%" (with-output-to-string (s) (yason:encode jobs s)))
        (is (= 1 (gethash "id" job)))
        (is (= 1 (gethash "id" (car jobs))))))))

(defun run-tests ()
  (let ((test-db (test-db-name "test-db")))
    (when (probe-file test-db)
      (delete-file test-db)))
  (run! 'sqlite-obj))


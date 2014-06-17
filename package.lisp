(defpackage :sqlite-obj
  (:use :cl :sqlite)
  (:nicknames :db)
  (:export :db-error
           :db-already-open
           :db-not-open
           :dbc
           :schema
           :db-open
           :db-close

           :apply-schema

           :db-get
           :db-insert
           :db-update
           :db-delete))


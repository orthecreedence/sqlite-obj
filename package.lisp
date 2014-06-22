(defpackage :sqlite-obj
  (:use :cl :sqlite)
  (:nicknames :db)
  (:export :db-error
           :db-already-open
           :db-not-open
           :dbc
           :schema
           :*db*
           :db-open
           :db-close

           :apply-schema

           :db-get
           :db-save
           :db-insert
           :db-update
           :db-delete
           :db-find
           :db-all))


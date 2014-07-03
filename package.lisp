(defpackage :sqlite-obj
  (:use :cl :sqlite)
  (:nicknames :db)
  (:export :db-error
           :*db*
           :db-open
           :db-close

           :db-missing-schema-entry
           :db-update-missing-id
           :apply-schema

           :db-get
           :db-save
           :db-insert
           :db-update
           :db-delete
           :db-find
           :db-all))


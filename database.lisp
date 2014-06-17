(in-package :sqlite-obj)

(define-condition db-error (simple-error)
  ((msg :initarg :msg :accessor db-error-msg :initform nil))
  (:report (lambda (c s) (format s "sqlite-obj error: ~a" (db-error-msg c))))
  (:documentation "General database error"))

(define-condition db-already-open (db-error) ()
  (:documentation "Thrown when a DB is being opened while another is open."))

(define-condition db-not-open (db-error) ()
  (:documentation "Thrown when operating on a closed database."))

(define-condition db-missing-schema-entry (db-error) ()
  (:documentation "Thrown when operating on a table that isn't in the current schema."))

(defclass database ()
  ((sqlite-db :accessor dbc :initarg :dbc :initform nil)
   (schema :accessor schema :initarg :schema :initform nil))
  (:documentation "Attaches a connection/schema to one object."))

(defun db-open (location)
  "Open a database."
  (ensure-directories-exist location)
  (vom:info "opening db (~a)" location)
  (make-instance 'database :dbc (sqlite:connect location)))

(defun db-close (db)
  "Close a database."
  (when (dbc db)
    (vom:info "closing db")
    (sqlite:disconnect (dbc db))))


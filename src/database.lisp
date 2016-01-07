(in-package :cl-user)
(defpackage crane.database
  (:use :cl)
  (:export :database
           :connect
           :connectedp
           :disconnect
           :sql-query
           :compile-statement
           :query
           :table-exists-p)
  (:documentation "Database base class and functionality."))
(in-package :crane.database)

(defclass database ()
  ((connection :accessor database-connection
               :initarg :connection
               :type dbi.driver:<dbi-connection>
               :documentation "The underlying connection. Unbound when it's not connected."))
  (:documentation "A database."))

(defgeneric connect (database)
  (:documentation "Connect to the database, returning t. If there's already a
  connection, return nil."))

(defgeneric connectedp (database)
  (:documentation "Whether or not the database is connected.")
  (:method ((db database))
    "The trivial default implementation."
    (slot-boundp db 'connection)))

(defgeneric disconnect (database)
  (:documentation "Disconnect from the database.")
  (:method ((database database))
    (dbi:disconnect (database-connection database))
    (slot-makunbound database 'connection)
    t))

(defgeneric sql-query (database sql arguments)
  (:documentation "Send SQL and interpolated arguments to a database. Returns a
  DBI result object.")

  (:method ((database database) (sql string) arguments)
    (if (connectedp database)
        (apply #'dbi:execute
               (cons (dbi:prepare (database-connection database)
                                  sql)
                     arguments))
        (error "Not connected"))))

(defgeneric compile-statement (database statement)
  (:documentation "Yield an SxQL statement, returning two values: an SQL string
  and a list of arguments to be interpolated into it.")

  (:method ((database database) statement)
    "The default implementation."
    (declare (ignore database))
    (multiple-value-bind (sql arguments)
        (let ((sxql:*quote-character* #\"))
          (sxql:yield statement))
      (cons sql arguments))))

(defgeneric query (database statement)
  (:documentation "Send an SxQL query.")

  (:method ((database database) statement)
    (let ((pair (compile-statement database statement)))
      (sql-query database (first pair) (rest pair)))))

(defgeneric table-exists-p (database table-name)
  (:documentation "Check whether a database table exists."))

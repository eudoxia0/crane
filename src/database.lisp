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
           :with-transaction
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
  (:documentation "Connects to the database, returning @c(T). If there's already
  a connection, returns @c(NIL) and does nothing.

@begin(deflist)

@term(Exceptional Situations)

@def(If the parameters are invalid, a driver-specific condition will be signalled.)

@term(See Also)

@def(@c(connectedp), @c(disconnect))

@end(deflist)"))

(defgeneric connectedp (database)
  (:documentation "Whether or not the database is connected.
@begin(deflist)

@term(Exceptional Situations)

@def(None.)

@end(deflist)")
  (:method ((db database))
    "The trivial default implementation."
    (slot-boundp db 'connection)))

(defgeneric disconnect (database)
  (:documentation "Disconnect from the database. Returns @c(T).

@begin(deflist)

@term(Exceptional Situations)

@def(None.)

@term(See Also)

@def(@c(connect), @c(connectedp))

@end(deflist)")
  (:method ((database database))
    (dbi:disconnect (database-connection database))
    (slot-makunbound database 'connection)
    t))

(defgeneric sql-query (database sql arguments)
  (:documentation "Send SQL and interpolated arguments to a database. Returns a
  DBI result object.")

  (:method ((database database) sql arguments)
    (format t "~%Sending SQL: ~A~%" sql)
    (if (connectedp database)
        (apply #'dbi:execute
               (cons (dbi:prepare (database-connection database)
                                  sql)
                     arguments))
        (error "Not connected"))))

(defgeneric compile-statement (database statement &key quote-character)
  (:documentation "Yield an SxQL statement, returning two values: an SQL string
  and a list of arguments to be interpolated into it.")

  (:method ((database database) statement &key (quote-character #\"))
    "The default implementation."
    (declare (ignore database))
    (let ((sxql:*quote-character* quote-character))
      (sxql:yield statement))))

(defgeneric query (database statement &key quote-character)
  (:documentation "Send an SxQL query.")

  (:method ((database database) statement &key (quote-character #\"))
    (multiple-value-bind (sql args)
        (compile-statement database
                           statement
                           :quote-character quote-character)
      (sql-query database sql args))))

(defmacro with-transaction ((database) &body body)
  "Execute @cl:param(body) within the context of a transaction."
  `(dbi:with-transaction (database-connection ,database)
     ,@body))

(defgeneric table-exists-p (database table-name)
  (:documentation "Check whether a database table exists.

Some database systems, notably Postgres and MySQL, support the information
schema interface. Other systems, like SQLite and Oracle, don't. Furthermore
there are subtle differences @i(among) systems which do support the information
schema. For instance, in MySQL the name of the table schema is just the name of
the database, while in Postgres is can be any schema name (but is 'public' by
default). This clearly introduces the need for a generic function to handle
these different cases portably."))

(in-package :cl-user)
(defpackage crane.database
  (:use :cl)
  ;; Classes
  (:export :database
           :postgres
           :mysql
           :sqlite3)
  ;; Accessors
  (:export :database-name
           :database-username
           :database-password
           :database-host
           :database-port
           :database-use-ssl)
  ;; Interface
  (:export :connect
           :connectedp
           :disconnect
           :send-sql
           :yield-statement
           :send-statement)
  ;; Constants
  (:export :*default-host*
           :*default-postgres-port*
           :*default-mysql-port*)
  (:documentation "Database classes, and associated functionality."))
(in-package :crane.database)

;;; Constants

(defvar *default-host* "localhost")

(defvar *default-postgres-port* 5432)

(defvar *default-mysql-port* 3306)

;;; Interface

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

(defgeneric send-sql (database sql arguments)
  (:documentation "Send SQL and interpolated arguments to a database. Returns a
  DBI result object.")
  (:method ((database database) (sql string) arguments)
    (if (connectedp database)
        (apply #'dbi:execute
               (cons (dbi:prepare (database-connection database)
                                  sql)
                     arguments))
        (error "Not connected"))))

(defgeneric yield-statement (database statement)
  (:documentation "Yield an SxQL statement into a (SQL string, arguments) cons.")
  (:method ((database database) statement)
    "The default implementation."
    (declare (ignore database))
    (multiple-value-bind (sql arguments)
        (let ((sxql:*quote-character* #\"))
          (sxql:yield statement))
      (cons sql arguments))))

(defgeneric send-statement (database statement)
  (:documentation "Yield an SxQL statement and send it to the database.")
  (:method ((database database) statement)
    (let ((pair (yield-statement database statement)))
      (send-sql database (first pair) (rest pair)))))

;;; Postgres

(defclass postgres (database)
  ((name :reader database-name
         :initarg :name
         :type string
         :documentation "The database name.")
   (username :reader database-username
             :initarg :username
             :type string
             :documentation "The username used to connect to the database.")
   (password :reader database-password
             :initarg :password
             :type string
             :documentation "The password used to connect to the database.")
   (host :reader database-host
         :initarg :host
         :initform *default-host*
         :type string
         :documentation "The host that runs the database server. 'localhost' by default.")
   (port :reader database-port
         :initarg :port
         :initform *default-postgres-port*
         :type integer
         :documentation "The port the database server listens on. 5432 by default.")
   (use-ssl :reader database-use-ssl
            :initarg :use-ssl
            :initform nil
            :type boolean
            :documentation "Whether or not to use SSL. Note: OpenSSL must be
            installed on the client. Default: NIL."))
  (:documentation "A Postgres database."))

(defmethod connect ((database postgres))
  "Connect to a Postgres database."
  (unless (connectedp database)
    (setf (database-connection database)
          (dbi:connect :postgres
                       :database-name (database-name database)
                       :username (database-username database)
                       :password (database-password database)
                       :host (database-host database)
                       :port (database-port database)
                       :use-ssl (if (database-use-ssl database)
                                    :yes
                                    :no)))
    t))

;;; MySQL

(defclass mysql (database)
  ((name :reader database-name
         :initarg :name
         :type string
         :documentation "The database name.")
   (username :reader database-username
             :initarg :username
             :type string
             :documentation "The username used to connect to the database.")
   (password :reader database-password
             :initarg :password
             :type string
             :documentation "The password used to connect to the database.")
   (host :reader database-host
         :initarg :host
         :initform *default-host*
         :type string
         :documentation "The host that runs the database server. 'localhost' by default.")
   (port :reader database-port
         :initarg :port
         :initform *default-mysql-port*
         :type integer
         :documentation "The port the database server listens on. 3306 by default."))
  (:documentation "A MySQL database."))

(defmethod connect ((database mysql))
  "Connect to a MySQL database."
  (unless (connectedp database)
    (setf (database-connection database)
          (dbi:connect :mysql
                       :database-name (database-name database)
                       :username (database-username database)
                       :password (database-password database)
                       :host (database-host database)
                       :port (database-port database)))
    t))

(defmethod connect :after ((database mysql))
  "Post-connect corrections."
  (send-sql database "SET SQL_MODE=ANSI_QUOTES" nil))

;;; SQLite3

(defclass sqlite3 (database)
  ((name :reader database-name
         :initarg :name
         :type string
         :documentation "The database file's absolute namestring."))
  (:documentation "An SQLite3 database."))

(defmethod connect ((database sqlite3))
  "Connect to a SQLite3 database."
  (unless (connectedp database)
    (setf (database-connection database)
          (dbi:connect :sqlite3
                       :database-name (database-name database)))
    t))

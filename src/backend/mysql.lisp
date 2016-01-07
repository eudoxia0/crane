(in-package :cl-user)
(defpackage crane.database.mysql
  (:use :cl)
  (:import-from :crane.database
	        :database
                :database-connection
                :connectedp
                :sql-query)
  (:import-from :crane.convert
                :lisp-to-database
                :database-to-lisp)
  (:export :*default-host*
           :*default-port*
           :mysql
           :database-name
           :database-username
           :database-password
           :database-host
           :database-port)
  (:documentation "MySQL support."))
(in-package :crane.database.mysql)

(defvar *default-host* "localhost"
  "The default host for MySQL databases.")

(defvar *default-port* 5432
  "The default port for MySQL databases.")

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
         :initform *default-port*
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
  (sql-query database "SET SQL_MODE=ANSI_QUOTES" nil))

;;; Value conversion

(defmethod lisp-to-database ((database mysql) (value t) (type crane.types:sql-type))
  value)

(defmethod database-to-lisp ((database mysql) (value t) (type crane.types:sql-type))
  value)

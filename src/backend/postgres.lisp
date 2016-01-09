(in-package :cl-user)
(defpackage crane.database.postgres
  (:use :cl)
  (:import-from :crane.database
	        :database
                :database-connection
                :connect
                :connectedp
                :sql-query
                :table-exists-p)
  (:import-from :crane.types
                :type-sql
                :column-id)
  (:import-from :crane.convert
                :lisp-to-database
                :database-to-lisp)
  (:export :*default-host*
           :*default-port*
           :postgres
           :database-name
           :database-username
           :database-password
           :database-host
           :database-port
           :database-use-ssl)
  (:documentation "Postgres support."))
(in-package :crane.database.postgres)

(defvar *default-host* "localhost"
  "The default host for Postgres databases.")

(defvar *default-port* 5432
  "The default port for Postgres databases.")

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
         :initform *default-port*
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

;;; Value conversion

(defmethod lisp-to-database ((database postgres) (value t))
  value)

(defmethod database-to-lisp ((database postgres) (value t) (type crane.types:sql-type))
  value)

;;; Other methods

(defmethod table-exists-p ((database postgres) table-name)
  "On Postgres, we can use the information schema to find whether the table
exists. Since table names are unqualified, they go into the public schema. There
may be a way to change this default, maybe the schema name should be an option
in the database object."
  (declare (type string table-name))
  (let* ((sql "SELECT table_name FROM INFORMATION_SCHEMA.TABLES WHERE table_schema = 'public' AND table_name = ?")
         (result (dbi:fetch-all (sql-query database
                                           sql
                                           (list
                                            (string-trim '(#\") table-name))))))
    (print result)
    (and result (stringp (second (first result))))))

;;; SQL types

(defmethod type-sql ((type column-id) (database postgres))
  "SERIAL")

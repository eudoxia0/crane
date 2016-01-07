(in-package :cl-user)
(defpackage crane.database.postgres
  (:use :cl)
  (:import-from :crane.database
	        :database
                :database-connection
                :connectedp)
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

(defmethod lisp-to-database ((database postgres) (value t) (type crane.types:sql-type))
  value)

(defmethod database-to-lisp ((database postgres) (value t) (type crane.types:sql-type))
  value)

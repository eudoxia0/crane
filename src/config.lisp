(in-package :cl-user)
(defpackage crane.config
  (:use :cl)
  (:export :*debug*
           :add-database
           :define-postgres-database
           :define-mysql-database
           :define-sqlite3-database
           :get-database
           :do-databases
           :list-databases)
  (:documentation "Configuration storage and management."))
(in-package :crane.config)

;;; Special Variables

(defvar *debug* t
  "Whether or not we are in debug mode.")

(defvar *database-registry* (list)
  "An association list of database tags to database instances.")

;;; Configuration definition

(defun add-database (tag database)
  "Add a database instance to the registry."
  (declare (type symbol tag)
           (type crane.database:database database))
  (push (cons tag database) *database-registry*))

(defmacro define-postgres-database (tag &key name username password
                                          (host crane.database.postgres:*default-host*)
                                          (port crane.database.postgres:*default-port*)
                                          (use-ssl nil))
  "Define a Postgres database."
  `(add-database ',tag
                 (make-instance 'crane.database.postgres:postgres
                                :name ,name
                                :username ,username
                                :password ,password
                                :host ,host
                                :port ,port
                                :use-ssl ,use-ssl)))

(defmacro define-mysql-database (tag &key name username password
                                       (host crane.database.mysql:*default-host*)
                                       (port crane.database.mysql:*default-port*))
  "Define a MySQL database."
  `(add-database ',tag
                 (make-instance 'crane.database.mysql:mysql
                                :name ,name
                                :username ,username
                                :password ,password
                                :host ,host
                                :port ,port)))

(defmacro define-sqlite3-database (tag &key (name ":memory:"))
  "Define an SQLite3 database."
  `(add-database ',tag
                 (make-instance 'crane.database.sqlite3:sqlite3
                                :name ,name)))

;;; Querying the database registry

(defun get-database (tag)
  "Find the database object associated to that tag."
  (declare (type symbol tag))
  (rest (assoc tag *database-registry*)))

(defmacro do-databases ((tag database) &body body)
  "Iterate over the databases."
  `(loop for (,tag . ,database) in *database-registry* do
     ,@body))

(defun list-databases (&optional (stream *standard-output*))
  "List the databases."
  (format t "~&Databases:")
  (do-databases (tag database)
    (format stream
            "~&  ~A: ~A"
            tag
            (typecase database
              (crane.database.postgres:postgres "Postgres")
              (crane.database.mysql:mysql "MySQL")
              (crane.database.sqlite3:sqlite3 "SQLite3")))))

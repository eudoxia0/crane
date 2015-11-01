(in-package :cl-user)
(defpackage crane.config
  (:use :cl)
  (:import-from :crane.database
                :*default-host*
                :*default-postgres-port*
                :*default-mysql-port*
                :postgres
                :mysql
                :sqlite3)
  (:export :*debug*
           :*default-database*
           :*migrations-directory*
           :with-configuration
           :configure
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

(defvar *default-database* nil
  "The tag of the default database.")

(defvar *migrations-directory*
  (merge-pathnames #p".crane-migrations"
                   (user-homedir-pathname))
  "The migrations directory.")

;;; Configuration definition

(defmacro with-configuration ((&key (default-database nil defaultp)
                                 (migrations-directory nil migrationsp)
                                 (debug t debugp))
                              &body body)
  "Execute the body with a local configuration."
  `(let ,(append
          (when defaultp
            `(*default-database* ,default-database))
          (when migrationsp
            `(*migrations-directory* ,migrations-directory))
          (when debugp
            `(*debug* ,debug)))
     ,@body))

(defun configure (&key (default-database nil defaultp)
                    (migrations-directory nil migrationsp)
                    (debug t debugp))
  "Set the global configuration. Returns T."
  (when defaultp
    (setf *default-database* default-database))
  (when migrationsp
    (setf *migrations-directory* migrations-directory))
  (when debugp
      (setf *debug* debug))
  t)

(defun add-database (tag instance)
  "Add a database instance to the registry."
  (push (cons tag instance) *database-registry*))

(defmacro define-postgres-database (tag &key name username password
                                          (host *default-host*)
                                          (port *default-postgres-port*)
                                          (use-ssl nil))
  "Define a Postgres database."
  `(add-database ',tag
                 (make-instance 'postgres
                                :name ,name
                                :username ,username
                                :password ,password
                                :host ,host
                                :port ,port
                                :use-ssl ,use-ssl)))

(defmacro define-mysql-database (tag &key name username password
                                       (host *default-host*)
                                       (port *default-mysql-port*))
  "Define a MySQL database."
  `(add-database ',tag
                 (make-instance 'mysql
                                :name ,name
                                :username ,username
                                :password ,password
                                :host ,host
                                :port ,port)))

(defmacro define-sqlite3-database (tag &key pathname)
  "Define an SQLite3 database."
  `(add-database ',tag
                 (make-instance 'sqlite3
                                :name (namestring ,pathname))))

;;; Querying the database registry

(defun get-database (tag)
  "Find the database object associated to that tag."
  (rest (assoc tag *database-registry*)))

(defmacro do-databases ((tag database) &body body)
  "Iterate over the databases."
  `(loop for (,tag . ,database) in *database-registry* do
     ,@body))

(defun list-databases (&optional (stream *standard-output*))
  "List the databases."
  (do-databases (tag database)
    (format stream
            "~& ~A ~A: ~A"
            (if (eq *default-database* tag)
                "*"
                " ")
            tag
            (typecase database
              (postgres "Postgres")
              (mysql "MySQL")
              (sqlite3 "SQLite3")))))

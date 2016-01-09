(in-package :cl-user)
(defpackage crane.session
  (:use :cl)
  (:import-from :crane.config
                :get-database)
  (:import-from :crane.database
                :database
                :connect
                :disconnect
                :table-exists-p
                :sql-query)
  (:import-from :crane.table
                :table-class
                :table-name)
  (:import-from :crane.table.sql
                :make-table-definition
                :table-definition-sql)
  (:export :*session*
           :session
           :make-session
           :session-databases
           :session-tables
           :session-migrate-p
           :session-migrations-directory
           :register-database
           :register-table
           :start
           :stop)
  (:documentation "Sessions tie table definitions, which are abstract and
  reusable, to specific databases."))
(in-package :crane.session)

(defvar *session*)

(defclass session ()
  ((databases :accessor session-databases
              :initarg :databases
              :type list
              :documentation "A list of database tags, representing the
              databases this session connects to.")
   (tables :accessor session-tables
           :initarg :tables
           :initform (make-hash-table :test #'eq)
           :type hash-table
           :documentation "A hash table from table names (symbols) to database
           tags (symbol). This associates the tables this sessin will manage
           with the databases where they will be created.")
   (migratep :reader session-migrate-p
             :initarg :migratep
             :initform t
             :type boolean
             :documentation "Whether or not automatic migrations are enabled.")
   (migrations-directory :reader session-migrations-directory
                         :initarg :migrations-directory
                         :type pathname
                         :documentation "The absolute pathname to the directory
                         where migrations data will be stored.")
   (%startedp :accessor session-started-p
              :initform nil
              :type boolean
              :documentation "Holds whether or not the session has been started."))
  (:documentation "A session ties table definitions, which are not tied to any
  particular database, to the actual databases where they will be created. It also manages the storage of migrations."))

(defun make-session (&key databases migratep (migrations-directory nil dirp)
                       defaultp)
  "Create a session object."
  (let ((instance (if dirp
                      (make-instance 'session
                                     :databases databases
                                     :migratep migratep
                                     :migrations-directory migrations-directory)
                      (make-instance 'session
                                     :migratep migratep
                                     :databases databases))))
    (when defaultp
      (setf *session* instance))
    instance))

(defun register-database (session database-tag)
  "Add a database tag to a session, so a connection will be created when the
  session is started.

Returns the database tag."
  (when (session-started-p session)
    (warn "Registering a database once a session has started will have no effect
      until it's restarted."))
  (push database-tag (session-databases session))
  database-tag)

(defun register-table (session table-name database-tag)
  "Register a table in the session, to be created in the given database.

Returns the table name."
  (when (session-started-p session)
    (warn "Registering a tabe when a session has already started will have no
    effect until the session is restarted."))
  (when (gethash table-name (session-tables session))
    (warn "This table has already been mapped to a database in the session."))
  (setf (gethash table-name (session-tables session)) database-tag)
  table-name)

(defmethod start ((session session))
  "Start the session, connecting to all the databases."
  (with-slots (databases tables migratep %startedp) session
    (unless %startedp
      ;; Iterate over the databases, connecting them
      (loop for tag in databases do
        (let ((db (get-database tag)))
          (when db
            (crane.database:connect db))))
      ;; Iterate over the tables, ensuring they exist
      (loop for table-name being the hash-keys of tables
            for database-tag being the hash-values of tables
            do
        (let ((table (find-class table-name))
              (database (get-database database-tag)))
          (when (and table database)
            (if migratep
                (error "Migrations not implemented yet :^)")
                (progn
                  ;; Check if the table exists
                  (unless (table-exists-p database (table-name table))
                    (format t "Creating table ~A" table-name)
                    (create-table table database))))))))
    (setf %startedp t))
  nil)

(defmethod stop ((session session))
  "Stop the session, closing all database connections."
  (loop for tag in (session-databases session) do
    (let ((db (get-database tag)))
      (when db
        (crane.database:disconnect db))))
  (setf (session-started-p session) nil)
  nil)

(defmethod create-table ((table table-class) (database database))
  "Create a table in a particular database."
  (mapcar #'(lambda (sql)
              (sql-query database sql nil))
          (table-definition-sql
           (make-table-definition table database))))

;; defmethod create-in-database ((database database) (instance standard-db-object))

;; defmethod create ((session session) (instance standard-db-object))

;; defmethod save ((session session) (instance standard-db-object))

;; defmethod delete ((session session) (instance standard-db-object))
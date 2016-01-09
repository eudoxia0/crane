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
                :sql-query
                :query
                :with-transaction)
  (:import-from :crane.table
                :table-class
                :table-name
                :table-columns
                :column-name
                :standard-db-object)
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
           :stop
           :create
           :exists-in-database-p
           :save
           :delete-instance
           :select
           :filter)
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

;;; Queries

(defun bound-slot-names (instance)
  "Given an instance of a standard-db-object object, return a list of symbols,
the names of its bound slots."
  (remove-if-not #'(lambda (slot)
                     (slot-boundp instance slot))
                 (mapcar #'column-name
                         (table-columns (class-of instance)))))

(defun insertable-plist (database instance)
  "Given an instance of standard-db-object, return a plist that can be sent to
SxQL for insertion. Conversion of Lisp values to database values happens here."
  (loop for name in (bound-slot-names instance) appending
    (list (alexandria:make-keyword name)
          (crane.convert:lisp-to-database database
                                          (slot-value instance name)))))

(defgeneric insert-into (database table-name plist)
  (:documentation "Insert a plist into the database, returning the ID.")

  (:method ((database crane.database.postgres:postgres) table-name plist)
    (declare (type symbol table-name)
             (type list plist))
    (second
     (dbi:fetch
      (query database
             (sxql:insert-into table-name
               (apply #'sxql.clause:make-clause
                      (cons :set= plist))
               (sxql:returning :id))))))

  (:method ((database crane.database.mysql:mysql) table-name plist)
    (declare (type symbol table-name)
             (type list plist))
    (with-transaction (database)
      (query database
             (sxql:insert-into table-name
                               (apply #'sxql.clause:make-clause
                                      (cons :set= plist))))
      (second (dbi:fetch (query database (sxql:select :|last_insert_id()|)
                                :quote-character nil)))))

  (:method ((database crane.database.sqlite3:sqlite3) table-name plist)
    (declare (type symbol table-name)
             (type list plist))
    (with-transaction (database)
      (query database
             (sxql:insert-into table-name
                               (apply #'sxql.clause:make-clause
                                      (cons :set= plist))))
      (second (dbi:fetch (query database (sxql:select :|last_insert_rowid()|)
                                :quote-character nil))))))

(defmethod create-in-database ((database database) (instance standard-db-object))
  "Given an instance of a database object, create it in the database."
  (insert-into database
               (class-name (class-of instance))
               (insertable-plist database instance)))

(defun database-for-instance (session instance)
  (declare (type standard-db-object instance))
  (get-database
   (gethash (class-name (class-of instance))
            (session-tables session))))

(defmethod exists-in-database-p ((session session) (instance standard-db-object))
  (when (slot-boundp instance 'crane.table:id)
    (let* ((query (sxql:select :id
                    (sxql:from (class-name (class-of instance)))
                    (sxql:where (:= :id (crane.table:id instance)))))
           (result (dbi:fetch (query (database-for-instance session instance)
                                     query))))
      (and result (integerp (second result)) t))))

(defmethod create ((session session) (instance standard-db-object))
  "Create an instance in the database the instance's class is associated to in
the session."
  (let ((id (create-in-database (database-for-instance session instance)
                                instance)))
    (setf (slot-value instance 'crane.table:id) id)
    instance))

(defmethod save ((session session) (instance standard-db-object))
  "Save an object in the database."
  (let ((database (database-for-instance session instance)))
    (query database
           (sxql:update (class-name (class-of instance))
             (apply #'sxql.clause:make-clause
                    (cons :set= (insertable-plist database instance)))
             (sxql:where (:= :id (crane.table:id instance))))))
  instance)

(defmethod delete-instance ((session session) (instance standard-db-object))
  "Delete an instance in the database. Return @c(nil)."
  (query (database-for-instance session instance)
         (sxql:delete-from (class-name (class-of instance))
           (sxql:where (:= :id (crane.table:id instance)))))
  nil)

(defun select (columns session class-name &rest arguments)
  "Execute a @c(SELECT) on a particular class."
  (query (crane.config:get-database (gethash class-name (session-tables session)))
         (sxql:select columns
           (sxql:from class-name)
           (sxql:make-clause :where (cons :and arguments)))))

(defun filter (session class-name &rest arguments)
  "Find instances of @c(class-name) that match the constraints in
@c(arguments)."
  (let* ((columns (mapcar #'alexandria:make-keyword
                          (mapcar #'column-name
                                  (table-columns (find-class class-name)))))
         (results (apply #'select (append (list columns session class-name)
                                          arguments))))
    (mapcar #'(lambda (result)
                (print result)
                t)
            (dbi:fetch-all results))))

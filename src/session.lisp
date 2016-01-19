(in-package :cl-user)
(defpackage crane.session
  (:use :cl)
  (:import-from :alexandria
                :curry)
  (:import-from :crane.config
                :get-database)
  (:import-from :crane.database
                :database
                :connect
                :disconnect
                :connectedp
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
           :session-migrate-p
           :session-migrations-directory
           :do-tables
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
  ((map :accessor session-map
        :initarg :map
        :initform (make-hash-table :test #'eq)
        :type hash-table
        :documentation "A hash table from table names (symbols) to database
objects. This associates the tables this session will manage with the databases
where they will be created.")
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
particular database, to the actual databases where they will be created. It also
manages the storage of migrations."))

(defun make-session (&key migratep (migrations-directory nil dirp) defaultp)
  "Create a session object."
  (let ((instance (if dirp
                      (make-instance 'session
                                     :migratep migratep
                                     :migrations-directory migrations-directory)
                      (make-instance 'session
                                     :migratep migratep))))
    (when defaultp
      (setf *session* instance))
    instance))

(defmacro do-tables ((table-name database session) &body body)
  "Iterate over the tables and databases in the session."
  `(maphash #'(lambda (,table-name ,database)
                (declare (type symbol ,table-name)
                         (type database ,database))
                ,@body)
            (session-map ,session)))

(defgeneric register-table (session table-name database)
  (:documentation "Register a table, and the database where it will be created,
  in the session.

Returns the @cl:param(table-name) argument.")

  (:method (session table-name (database symbol))
    "Add a database by tag name."
    (declare (type session session)
             (type symbol table-name))
    (register-table session table-name (get-database database)))

  (:method (session table-name (database database))
    "Add a database object."
    (when (session-started-p session)
      (warn "Registering a table when a session has already started will have no effect until the session is restarted."))
    (when (gethash table-name (session-map session))
      (warn "This table has already been mapped to a database in the session."))
    (setf (gethash table-name (session-map session)) database)
    table-name))

(defmethod start ((session session))
  "Start the session, connecting to all the databases."
  (with-slots (databases tables migratep %startedp) session
    (unless %startedp
      (do-tables (table-name database session)
        (connect database)
        (let ((table-class (find-class table-name)))
          (if migratep
              (error "Migrations not implemented yet :^)")
              (progn
                ;; Check if the table exists
                (unless (table-exists-p database (table-name table-class))
                  (create-table table-class database)))))))
    (setf %startedp t))
  nil)

(defmethod stop ((session session))
  "Stop the session, closing all database connections."
  (do-tables (table-name database session)
    (declare (ignore table-name))
    (unless (connectedp database)
      (disconnect database)))
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
  (gethash (class-name (class-of instance))
           (session-map session)))

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

(defun database-for-class (session class-name)
  (gethash class-name (session-map session)))

(defun select (columns session class-name &rest arguments)
  "Execute a @c(SELECT) on a particular class."
  (query (database-for-class session class-name)
         (if arguments
             (apply #'sxql.statement:make-statement
                    (append (list :select
                                  (sxql::convert-if-fields-clause columns)
                                  (sxql:from class-name))
                            (mapcar (curry #'apply #'sxql:make-clause)
                                    arguments)))
             (sxql:select columns
               (sxql:from class-name)))))

(defun sql-keyword-to-initarg (keyword)
  (let ((*package* (find-package :keyword)))
    (read-from-string (symbol-name keyword))))

(defun column-names (table-name)
  "Return a list of column names for table name."
  (mapcar #'column-name
          (table-columns (find-class table-name))))

(defun sql-plist-to-object (database class-name plist)
  "Convert an SQL plist to an instance."
  (let ((instance (make-instance class-name)))
    (loop for (key value) on plist by #'cddr
          for slot-name in (column-names class-name)
          do
      (let ((slot-type (crane.table:slot-type class-name slot-name)))
        (setf (slot-value instance slot-name)
              (crane.convert:database-to-lisp database value slot-type))))
    instance))

(defun filter (session class-name &rest arguments)
  "Find instances of @c(class-name) that match the constraints in
@c(arguments)."
  (let* ((columns (mapcar #'alexandria:make-keyword (column-names class-name)))
         (results (apply #'select (append (list columns session class-name)
                                          arguments)))
         (database (database-for-class session class-name)))
    (mapcar (alexandria:curry #'sql-plist-to-object database class-name)
            (dbi:fetch-all results))))

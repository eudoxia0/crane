(in-package :cl-user)
(defpackage crane.database.sqlite3
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
                :column-id
                :bool)
  (:import-from :crane.convert
                :lisp-to-database
                :database-to-lisp)
  (:export :*default-host*
           :*default-port*
           :sqlite3
           :database-name)
  (:documentation "SQLite3 support."))
(in-package :crane.database.sqlite3)

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

(defmethod connect :after ((database sqlite3))
  "Post-connect corrections."
  (sql-query database "PRAGMA foreign_keys = ON;" nil))

;;; Value conversion

(defmethod lisp-to-database ((database sqlite3) (value t))
  value)

(defmethod database-to-lisp ((database sqlite3) (value t) (type crane.types:sql-type))
  value)

(defmethod database-to-lisp ((database sqlite3) (value integer) (type bool))
  (if (= value 1)
      t
      nil))

;;; Other methods

(defmethod table-exists-p ((database sqlite3) table-name)
  "SQLite3 doesn't support the information schema, so we have to use custom
SQL."
  (declare (type string table-name))
  (let* ((sql "SELECT name FROM sqlite_master WHERE type='table' AND name=?")
         (result (dbi:fetch-all (sql-query database
                                           sql
                                           (list
                                            (string-trim '(#\") table-name))))))
    (and result (stringp (getf (first result) :|name|)) t)))

;;; SQL types

(defmethod type-sql ((type column-id) (database sqlite3))
  "INTEGER")

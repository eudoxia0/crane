(defpackage crane.connect
  (:use :cl :anaphora :iter)
  (:import-from :alexandria
                :remove-from-plist)
  (:import-from :sxql
                :*quote-character*)
  (:export :<database>
           :database-type
           :database-name
           :database-connection
           :*default-db*
           :connect
           :disconnect
           :get-db
           :get-connection)
  (:documentation "Handles database connections, connection parameter validation, and various low-level DB-specific modes."))
(in-package :crane.connect)

;; Postgres and SQLite both use the double-quote as an escape character. MySQL
;; lets you do it as well, but requires setting `SQL_MODE=ANSI_QUOTES;`. MS SQL
;; Server uses brackets, but can be made to accept double-quotes by setting
;; QUOTED_IDENTIFIER ON
(setf *quote-character* #\")

(defun set-proper-quote-character (connection database-type)
  (cond
    ((eq database-type :mysql)
     (dbi:execute (dbi:prepare connection "SET SQL_MODE=ANSI_QUOTES")))
    (t
     ;; Postgres or SQLite, do nothing
     t)))

(defun enforce-foreign-keys (connection database-type)
  (cond
    ((eq database-type :sqlite3)
     (dbi:execute (dbi:prepare connection "PRAGMA foreign_keys = ON;")))
    (t
     ;; Postgres or MySQL, do nothing
     t)))

(defparameter +system-mapping+
  (list :postgres :dbd-postgres
        :sqlite3  :dbd-sqlite3
        :mysql    :dbd-mysql))

(defun load-driver (driver)
  "Load the ASDF system for the specified database module."
  #+quicklisp (ql:quickload driver :verbose nil)
  #-quicklisp (asdf:load-system driver :verbose nil))

(defparameter +db-params+
  '(:postgres ((:name :database-name)
               (:user :username)
               (:pass :password)
               (:host :host "localhost")
               (:port :port 5432)
               (:ssl  :use-ssl :no))
    :sqlite3 ((:name :database-name))
    :mysql ((:name :database-name)
            (:user :username)
            (:pass :password)
            (:host :host "localhost")
            (:port :port 3306))))

(defun validate-connection-spec (db database-type spec)
  (let* ((reference-spec (getf +db-params+ database-type))
         (normalized-spec
           ;; The reference spec without default values
           (iter (for line in reference-spec)
                 (appending (list (car line) (cadr line)))))
         (required-keys
           (iter (for line in reference-spec)
                 (if (not (cddr line))
                     (collecting (cadr line)))))
         (final-spec (list)))
    (iter (for key in spec by #'cddr)
          (aif (getf normalized-spec key)
               (setf (getf final-spec it) (getf spec key))
               (error 'crane.errors:configuration-error
                      :key (list :databases :-> db :-> key)
                      :text (format nil "The property '~A' is not supported by
the connection spec of the database '~A'" key db))))
    (aif (set-difference
          required-keys
          (crane.util:plist-keys final-spec))
         (error 'crane.errors:configuration-error
                :key (list :databases :-> db)
                :text (format nil "The following properties of the connection
spec for the database '~A' have not been provided: ~A" db it))
         final-spec)))

(defparameter *db* (make-hash-table)
  "A map from database names to <database> objects.")

(defclass <database> ()
  ((type :reader database-type
         :initarg :type
         :type keyword
         :documentation "A keyword representing the database type, e.g :sqlite3, :postgres.")
   (name :reader database-name
         :initarg :name
         :type string
         :documentation "The database name. If it's an SQLite3 database, must be the pathname's namestring.")
   (conn-spec :reader database-connection-spec
              :initarg :conn-spec
              :documentation "The connection specification.")
   (conn :accessor database-connection
         :initarg :connection
         :documentation "The underlying connection object."))
  (:documentation "A database."))

(defparameter *default-db* nil
  "The name of the default database")

(defun validate-all-databases ()
  "Immediately after configuration, iterate over the list of defined databases,
validating configuration parameters, creating their corresponding <database>
instances, and setting the value of *default-db*."
  (let ((databases (crane.config:get-config-value :databases)))
    (iter (for (db spec) on databases by #'cddr)
      (let ((name (getf spec :name))
            (type (getf spec :type)))
        (aif (getf +system-mapping+ type)
             (let ((database (make-instance '<database>
                                            :name name
                                            :type type
                                            :conn-spec spec)))
               (load-driver it)
               (setf (gethash db *db*) database))
             (error 'crane.errors:configuration-error
                    :key (list :databases :-> db)
                    :text (format nil
                                  "The database type '~A' is not supported by DBI yet."
                                  type)))))
    (setf *default-db* (first databases))))

(setf crane.config:*after-config-hook*
      #'validate-all-databases)

(defmethod make-connection ((database <database>))
  (let* ((spec (database-connection-spec database))
         (type (getf spec :type))
         (conn (apply #'dbi:connect
                      (cons type
                            (validate-connection-spec database
                                                      type
                                                      (remove-from-plist spec :type))))))
    (set-proper-quote-character conn type)
    (enforce-foreign-keys conn type)
    (setf (database-connection database) conn)))

(defun connect ()
  "Connect to all the databases specified in the configuration."
  (loop for db being the hash-values in *db* do
    (make-connection db)))

(defun disconnect ()
  "Cut all connections."
  (loop for db being the hash-values in *db* do
    (dbi:disconnect (database-connection db))))

(defun get-db (&optional database-name)
  "Return the database matching a specific name"
  (gethash (aif database-name it *default-db*) *db*))

(defun get-connection (&optional database-name)
  "Return the connection handler for a given database."
  (database-connection (get-db database-name)))

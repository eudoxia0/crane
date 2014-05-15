(defpackage :crane.connect
  (:use :cl :anaphora :iter :cl-annot.doc)
  (:import-from :alexandria
                :remove-from-plist)
  (:import-from :sxql
                :*quote-character*))
(in-package :crane.connect)
(annot:enable-annot-syntax)

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

(defparameter +system-mapping+
  (list :postgres :dbd-postgres
        :sqlite3  :dbd-sqlite3
        :mysql    :dbd-mysql))

@doc "Load the ASDF system for the specified database module."
(defun load-driver (driver)
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
          (crane.utils:plist-keys final-spec))
         (error 'crane.errors:configuration-error
                :key (list :databases :-> db)
                :text (format nil "The following properties of the connection
spec for the database '~A' have not been provided: ~A" db it))
         final-spec)))

@doc "A map from database names to connections."
(defparameter *db* (make-hash-table))

@doc "The name of the default database"
@export
(defparameter *default-db* nil)

(defun connect-spec (db spec)
  (let ((type (getf spec :type)))
    (aif (getf +system-mapping+ type)
         (progn
           (load-driver it)
           (let ((connection
                   (apply #'dbi:connect
                          (cons type
                                (validate-connection-spec db
                                                          type
                                                          (remove-from-plist spec :type))))))
             (set-proper-quote-character connection type)
             connection))
         (error 'crane.errors:configuration-error
                :key (list :databases :-> db)
                :text (format nil
                              "The database type '~A' is not supported by DBI yet."
                              type)))))

@doc "Connect to all the databases specified in the configuration."
@export
(defun connect ()
  (aif (crane.config:get-config-value :databases)
       (progn
         (iter (for (db spec) on it by #'cddr)
               (setf (gethash db *db*) (connect-spec db spec)))
         (setf *default-db* (car it)))
       (error 'crane.errors:configuration-error
              :key :databases
              :text "No databases found.")))

@doc "Return the connection handler for a given database."
@export
(defun get-connection (&optional database-name)
  (gethash (aif database-name it *default-db*) *db*))

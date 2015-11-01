(in-package :cl-user)
(defpackage crane.types
  (:use :cl)
  ;; Classes
  (:export :sql-type
           :int
           :bigint
           :smallint
           :numeric
           :double
           :text
           :varchar
           :timestamp
           :datetime)
  ;; Accessors
  (:export :varchar-length)
  ;; Interface
  (:export :type-sql)
  (:documentation "Implements the database types."))
(in-package :crane.types)

;;; Interface

(defclass sql-type ()
  ()
  (:documentation "The base class of all SQL types."))

(defgeneric type-sql (type)
  (:documentation "Return the SQL string that denotes a type."))

;;; Types

;; Numeric types

(defclass int (sql-type)
  ()
  (:documentation "An integer"))

(defclass bigint (sql-type)
  ()
  (:documentation "A big integer."))

(defclass smallint (sql-type)
  ()
  (:documentation "A small integer."))

(defclass numeric (sql-type)
  ()
  (:documentation "A number."))

(defclass double (sql-type)
  ()
  (:documentation "A double-precision floating-point number."))

;; Text

(defclass text (sql-type)
  ()
  (:documentation "A piece of text."))

(defclass varchar (sql-type)
  ((length :reader varchar-length
           :initarg :length
           :type integer
           :documentation "The length of the string."))
  (:documentation "A variable-length string."))

;; Time

(defclass timestamp (sql-type)
  ()
  (:documentation "A timestamp."))

(defclass datetime (sql-type)
  ()
  (:documentation "A date/time value."))

;;; SQL

(macrolet ((type-name (class name)
             `(defmethod type-sql ((type ,class))
                ,(format nil "Type string of the ~A type." (string-downcase class))
                (declare (ignore type))
                ,name)))
  (type-name int "INTEGER")
  (type-name bigint "BIGINT")
  (type-name smallint "SMALLINT")
  (type-name numeric "NUMERIC")
  (type-name double "DOUBLE")
  (type-name text "TEXT")
  (type-name varchar "VARCHAR")
  (type-name timestamp "TIMESTAMP")
  (type-name datetime "DATETIME"))

(defmethod type-sql ((type varchar))
  "Type string of the varchar type."
  (format nil "VARCHAR(~D)" (varchar-length type)))

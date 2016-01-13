(in-package :cl-user)
(defpackage crane.convert
  (:use :cl)
  (:export :lisp-to-database
           :database-to-lisp)
  (:documentation "Convert between Lisp and SQL values."))
(in-package :crane.convert)

;;; Generics

(defgeneric lisp-to-database (database value)
  (:documentation "Convert a Lisp value to a value appropriate for the given
  database, given the SQL type."))

(defgeneric database-to-lisp (database value type)
  (:documentation "Convert a database value to a Lisp value, given the SQL
  type."))

;;; Default implementation

(defmethod lisp-to-database ((database crane.database:database)
                             (value local-time:timestamp))
  (declare (ignore database))
  (local-time:timestamp-to-universal value))

(defmethod database-to-lisp ((database crane.database:database)
                             (value integer)
                             (type crane.types:timestamp))
  (declare (ignore database type))
  (local-time:universal-to-timestamp value))

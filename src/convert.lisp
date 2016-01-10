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
  database, given the SQL type.")

  (:method ((database t) (value (eql t)))
    1))

(defgeneric database-to-lisp (database value type)
  (:documentation "Convert a database value to a Lisp value, given the SQL
  type."))

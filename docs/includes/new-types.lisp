(in-package :cl-user)
(defpackage crane.ext.json
  (:use :cl)
  (:import-from :crane.types
                :sql-type
                :type-sql)
  (:import-from :crane.convert
                :lisp-to-database
                :database-to-lisp)
  (:import-from :crane.database
                :database)
  (:export :json)
  (:documentation "Add a portable JSON column type."))
(in-package :crane.ext.json)

;;; Type definition

(defclass json (sql-type)
  ()
  (:documentation "A JSON column."))

(defmethod type-sql ((type json) (database database))
  ;; We just call type-sql on the TEXT type.  Another approach would be to
  ;; subclass the `text` class, but I wanted to show the method definition.
  (type-sql (make-instance 'crane.types:text) database))

;;; Conversion

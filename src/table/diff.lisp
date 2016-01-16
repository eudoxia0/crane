(in-package :cl-user)
(defpackage crane.table.diff
  (:use :cl)
  (:import-from :crane.table.serialize
                :storable-table)
  (:documentation "Tools for computing the difference between two table definitions."))
(in-package :crane.table.diff)

(defclass difference ()
  ((new-columns :reader new-columns
                :initarg :new-columns
                :type list
                :documentation "A list of column objects that are to be added the table.")
   (old-columns :reader old-columns
                :initarg :old-columns
                :type list
                :documentation "A list of column objects that are to be dropped."))
  (:documentation "Represents the difference between two tables."))

(defun differences (past present)
  "Compute the differences between two table definitions."
  (declare (type storable-table past present)
           (ignore past present))
  t)

(in-package :cl-user)
(defpackage crane.table.diff
  (:use :cl)
  (:import-from :crane.table
                :column
                :column-name)
  (:import-from :crane.table.serialize
                :storable-table
                :table-columns)
  (:export :difference
           :new-columns
           :old-columns
           :differences)
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

(defun column= (a b)
  (eq (column-name a) (column-name b)))

(defun has-column-p (column table)
  (not (null (position column table :test #'column=))))

(defun differences (past present)
  "Compute the differences between two table definitions."
  (declare (type storable-table past present))
  (make-instance 'difference
                 :new-columns (remove-if #'(lambda (column)
                                             (has-column-p column (table-columns past)))
                                         (table-columns present))
                 :old-columns (remove-if #'(lambda (column)
                                             (has-column-p column
                                                           (table-columns present)))
                                         (table-columns past))))

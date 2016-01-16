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
                :documentation "A list of column objects that are to be dropped.")
   (old-constraints :reader old-constraints
                    :initarg :old-constraints
                    :type list
                    :documentation "A list of constraints that existed in the old table.")
   (new-constraints :reader new-constraints
                    :initarg :new-constraints
                    :type list
                    :documentation "A list of constraints that only exist in the new table.")
   (old-indices :reader old-indices
                :initarg :old-indices
                :type list
                :documentation "A list of old indices.")
   (new-indices :reader new-indices
                :initarg :new-indices
                :type list
                :documentation "A list of new indices."))
  (:documentation "Represents the difference between two tables."))

(defun column= (a b)
  (eq (column-name a) (column-name b)))

(defun has-column-p (column table)
  (not (null (position column table :test #'column=))))

(defun column-difference (past present)
  (declare (type storable-table past present))
  (let ((past-columns (table-columns past))
        (present-columns (table-columns present)))
    (values (remove-if #'(lambda (column)
                           (has-column-p column past-columns))
                       present-columns)
            (remove-if #'(lambda (column)
                           (has-column-p column present-columns))
                       past-columns))))

(defun differences (past present)
  "Compute the differences between two table definitions."
  (declare (type storable-table past present))
  (multiple-value-bind (new-columns old-columns)
      (column-difference past present)
    (make-instance 'difference
                   :new-columns new-columns
                   :old-columns old-columns)))

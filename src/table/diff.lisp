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
           :new-constraints
           :old-constraints
           :new-indices
           :old-indices
           :differences
           :has-changes-p)
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

(defun thing-difference (past present &key key test)
  (declare (type storable-table past present))
  (let ((past-things (funcall key past))
        (present-things (funcall key present)))
    (flet ((has-thing-p (thing sequence)
             (not (null (position thing sequence :test test)))))
      (values (remove-if #'(lambda (thing)
                             (has-thing-p thing past-things))
                         present-things)
              (remove-if #'(lambda (thing)
                             (has-thing-p thing present-things))
                         past-things)))))

(defun column-difference (past present)
  (declare (type storable-table past present))
  (thing-difference past
                    present
                    :key #'table-columns
                    :test #'(lambda (a b)
                              (eq (column-name a) (column-name b)))))

(defun constraint-difference (past present)
  (declare (type storable-table past present))
  (thing-difference past
                    present
                    :key #'crane.table.sql:table-constraints
                    :test #'crane.table.sql:constraint=))

(defun index-difference (past present)
  (declare (type storable-table past present))
  (thing-difference past
                    present
                    :key #'crane.table.sql:table-indices
                    :test #'crane.table.sql:index=))

(defun differences (past present)
  "Compute the differences between two table definitions."
  (declare (type storable-table past present))
  (multiple-value-bind (new-columns old-columns)
      (column-difference past present)
    (multiple-value-bind (new-constraints old-constraints)
        (constraint-difference past present)
      (multiple-value-bind (new-indices old-indices)
          (index-difference past present)
        (make-instance 'difference
                       :new-columns new-columns
                       :old-columns old-columns
                       :new-constraints new-constraints
                       :old-constraints old-constraints
                       :new-indices new-indices
                       :old-indices old-indices)))))

(defun has-changes-p (difference)
  "Does this difference object have any actual differences?"
  (and (new-columns difference)
       (old-columns difference)
       (new-constraints difference)
       (old-constraints difference)
       (new-indices difference)
       (old-indices difference)
       t))

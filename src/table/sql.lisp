(in-package :cl-user)
(defpackage crane.table.sql
  (:use :cl)
  (:import-from :crane.table
                ;; Column
                :column
                :column-null-p
                :column-unique-p
                :column-primary-p
                :column-index-p
                :column-foreign
                :column-autoincrement-p
                ;; Foreign key
                :foreign-key-table
                :foreign-key-on-delete
                :foreign-key-on-update
                :referential-action-name)
  (:documentation "A tiny DSL for building the SQL code to generate and alter
  tables. This is necessary because of SxQL's limitations."))
(in-package :crane.table.sql)

;;; Utilities

(defun symbol-to-sql (symbol)
  "Return a valid SQL string equivalent of a Lisp symbol. Identical to SxQL's
behaviour."
  (let ((sxql:*quote-character* #\"))
    (sxql:yield (sxql.operator:detect-and-convert symbol))))

;;; Constraints

(defclass constraint ()
  ()
  (:documentation "The base class of all constraints."))

(defclass single-column (constraint)
  ((column :reader constraint-column
           :initarg :column
           :type string
           :documentation "The name of the column."))
  (:documentation "Single-column constraints."))

(defclass multi-column (constraint)
  ((columns :reader constraint-columns
            :initarg :columns
            :initform nil
            :type list
            :documentation "A list of column names (strings)."))
  (:documentation "Multi-column constraints."))

(defclass unique (multi-column)
  ()
  (:documentation "Represents a @c(UNIQUE) constraint."))

(defclass not-null (single-column)
  ()
  (:documentation "Represents a @c(NOT NULL) constraint."))

(defclass primary-key (multi-column)
  ()
  (:documentation "Represents a @c(PRIMARY KEY) constraint."))

(defclass index (single-column)
  ()
  (:documentation "Represents an @c(INDEX) constraint."))

(defclass foreign-key (constraint)
  ((column :reader constraint-column
           :initarg :column
           :type string
           :documentation "The column that will act as the foreign key.")
   (foreign-table :reader constraint-foreign-table
                  :initarg :foreign-table
                  :type string
                  :documentation "The name of the table being referenced.")
   (foreign-column :reader constraint-foreign-column
                   :initarg :foreign-column
                   :type string
                   :documentation "The name of the column being referened.")
   (on-delete :reader constraint-on-delete
              :initarg :on-delete
              :type string
              :documentation "A string describing the @c(ON DELETE) action.")
   (on-update :reader constraint-on-update
              :initarg :on-update
              :type string
              :documentation "A string describing the @c(ON UPDATE) action."))
  (:documentation "Represents a @c(FOREIGN KEY) constraint."))

(defgeneric add-constraint (constraint name table-name)
  (:documentation "Return a string with the SQL required to add the constraint
  to the table.

For instance, a @c(not-null) constraint on a column @c(\"username\") with name
@c(\"non-null\") and table name @c(\"user\") should generate something like
@c(\"ALTER TABLE \"user\" ADD CONSTRAINT \"not-null\" CHECK (\"username\") IS
NOT NULL\").

The @cl:param(name) and @cl:param(table-name) arguments are strings."))

(defgeneric drop-constraint (constraint name table-name)
  (:documentation "Return a string with the SQL required to drop the constraint
  from the table.

The types of the arguments are as in @c(add-constraint)."))

(defun alter-table-add (table-name constraint-name)
  "Return an SQL string with the first part of an @c(ALTER TABLE ADD CONSTRAINT)
statement."
  (declare (type symbol table-name)
           (type string constraint-name))
  (format nil "ALTER TABLE ~A ADD CONSTRAINT ~A "
          table-name
          constraint-name))

(defun alter-table-drop (table-name constraint-name)
  "Return SQL to drop a constraint."
  (declare (type symbol table-name)
           (type string constraint-name))
  (format nil "ALTER TABLE ~A DROP CONSTRAINT ~A"
          table-name
          constraint-name))

(defmethod add-constraint ((constraint unique) name table-name)
  "Add a @c(UNIQUE) constraint."
  (format nil "~A UNIQUE (~{~A~^, ~})"
          (alter-table-add table-name name)
          (constraint-columns constraint)))

(defmethod drop-condtraint ((constraint unique) name table-name)
  "Drop a @c(UNIQUE) constraint."
  (declare (ignore constraint))
  (alter-table-drop table-name name))

(defmethod add-constraint ((constraint not-null) name table-name)
  "Add a @c(NOT NULL) constraint."
  (format nil "~A CHECK (~A IS NOT NULL)"
          (alter-table-add table-name name)
          (constraint-column constraint)))

(defmethod drop-condtraint ((constraint not-null) name table-name)
  "Drop a @c(NOT NULL) constraint."
  (declare (ignore constraint))
  (alter-table-drop table-name name))

(defmethod add-constraint ((constraint primary-key) name table-name)
  "Add a @c(PRIMARY KEY) constraint."
  (format nil "~A PRIMARY KEY (~{~A~^, ~})"
          (alter-table-add table-name name)
          (constraint-columns constraint)))

(defmethod drop-condtraint ((constraint primary-key) name table-name)
  "Drop a @c(PRIMARY) constraint."
  (declare (ignore constraint))
  (alter-table-drop table-name name))

(defmethod add-constraint ((constraint index) name table-name)
  "Add an @c(INDEX) constraint."
  (format nil "CREATE INDEX ~A on ~A (~A)"
          name
          table-name
          (constraint-column constraint)))

(defmethod drop-constraint ((constraint index) name table-name)
  "Drop an @c(INDEX) constraint."
  (declare (ignore constraint))
  (format nil "DROP INDEX ~A on ~A"
          name
          table-name))

;;; Extract info from tables

(defun column-constraints (name column)
  "Extract a list of constraints from a column."
  (declare (type symbol name)
           (type column column))
  (remove-if #'null
             (list
              (when (column-null-p column)
                (make-instance 'not-null :column (symbol-to-sql name)))
              (when (column-unique-p column)
                (make-instance 'unique :columns (list (symbol-to-sql name))))
              (when (column-primary-p column)
                (make-instance 'primary-key :column (symbol-to-sql name)))
              (when (column-index-p column)
                (make-instance 'index :column (symbol-to-sql name)))
              (when (slot-boundp column 'column-foreign)
                (let ((foreign (column-foreign column)))
                  (make-instance 'foreign
                                 :column (symbol-to-sql name)
                                 :foreign-table (symbol-to-sql (foreign-key-table foreign))
                                 :foreign-column (symbol-to-sql 'crane.table:id)
                                 :on-delete (foreign-key-on-delete foreign)
                                 :on-update (foreign-key-on-update foreign)))))))

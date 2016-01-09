(in-package :cl-user)
(defpackage crane.table.sql
  (:use :cl)
  (:import-from :crane.util
                :symbol-to-sql)
  (:import-from :crane.database
                :database)
  (:import-from :crane.types
                :type-sql)
  (:import-from :crane.table
                ;; Column
                :column
                :column-name
                :column-type
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
                :referential-action-name
                ;; Table
                :table-class
                :table-name
                :table-columns)
  (:documentation "A tiny DSL for building the SQL code to generate and alter
  tables. This is necessary because of SxQL's limitations."))
(in-package :crane.table.sql)

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

;;; Constraint methods

(defgeneric constraint-partial-name (constraint)
  (:documentation "Given a constraint, return a string that will be used as part
  of its name to make it more human readable.")

  (:method ((constraint unique))
    (declare (ignore constraint))
    "unique")

  (:method ((constraint not-null))
    (declare (ignore constraint))
    "non-null")

  (:method ((constraint primary-key))
    (declare (ignore constraint))
    "primary")

  (:method ((constraint foreign-key))
    "foreign"))

(defgeneric constraint-sql (constraint)
  (:documentation "Return a string with the SQL representing the constraint.

This is the text after a @c(CONSTRAINT [name]) declaration in a @c(CREATE
TABLE), or after an @c(ALTER TABLE [table] ADD CONSTRAINT) statement.")

  (:method ((constraint unique))
    (with-slots (columns) constraint
      (format nil "UNIQUE (~{~A~^, ~})" columns)))

  (:method ((constraint not-null))
    (with-slots (column) constraint
      (format nil "CHECK (~A IS NOT NULL)" column)))

  (:method ((constraint primary-key))
    (with-slots (columns) constraint
      (format nil "PRIMARY KEY (~{~A~^, ~})" columns)))

  (:method ((constraint foreign-key))
    (with-slots (column foreign-table foreign-column on-delete on-update) constraint
      (format nil "FOREIGN KEY (~A) REFERENCES ~A(~A) ON DELETE ~A ON UPDATE ~A"
              column
              foreign-table
              foreign-column
              on-delete
              on-update))))

(defun render-constraint (constraint name)
  "Given a constraint, and its name, return an SQL string ready for inclusion in
a @c(CREATE TABLE) statement."
  (declare (type constraint constraint)
           (type string name))
  (format nil "CONSTRAINT ~A ~A" name (constraint-sql constraint)))

;;; Indices

(defclass index ()
  ((column :reader index-column
           :initarg :column
           :type string
           :documentation "The SQL name of the column to be indexed."))
  (:documentation "Represents an index."))

(defun add-index (index index-name table-name)
  "Given an index, its name and the name of the table it belongs to, return the
SQL statement to create it."
  (with-slots (column) index
    (format nil "CREATE INDEX ~A ON ~A(~A)" index-name table-name column)))

(defun drop-index (index-name table-name)
  "Given an index, its name and the name of the table it belongs to, return the
SQL statement to drop it."
  (format nil "DROP INDEX ~A ON ~A" index-name table-name))

;;; Table definition

(defclass column-definition ()
  ((name :reader column-name
         :initarg :name
         :type string
         :documentation "The column's SQL name, a string.")
   (type :reader column-type
         :initarg :type
         :type string
         :documentation "The column SQL type, a string."))
  (:documentation "All the information needed to define an SQL column."))

(defclass table-definition ()
  ((name :reader table-definition-name
         :initarg :name
         :type string
         :documentation "The table's SQL name.")
   (columns :reader table-definition-columns
            :initarg :columns
            :type list
            :documentation "A list of column definitions.")
   (constraints :reader table-definition-constraints
                :initarg :constraints
                :type list
                :documentation "A list of constraint objects.")
   (indices :reader table-definition-indices
            :initarg :indices
            :type list
            :documentation "A list of indices."))
  (:documentation "All the information needed to create a table."))

;;; Extract info from tables

(defun column-constraints (column)
  "Extract a list of constraints from a column."
  (declare (type column column))
  (let ((name (column-name column)))
    (remove-if #'null
               (list
                (when (column-null-p column)
                  (make-instance 'not-null :column (symbol-to-sql name)))
                (when (column-unique-p column)
                  (make-instance 'unique :columns (list (symbol-to-sql name))))
                (when (column-primary-p column)
                  (make-instance 'primary-key :columns (list (symbol-to-sql name))))
                (when (slot-boundp column 'column-foreign)
                  (let ((foreign (column-foreign column)))
                    (make-instance 'foreign
                                   :column (symbol-to-sql name)
                                   :foreign-table (symbol-to-sql (foreign-key-table foreign))
                                   :foreign-column (symbol-to-sql 'crane.table:id)
                                   :on-delete (foreign-key-on-delete foreign)
                                   :on-update (foreign-key-on-update foreign))))))))

(defun table-constraints (table-class)
  "Extract a list of contraints from a table."
  (declare (type table-class table-class))
  (loop for column in (table-columns table-class) appending
    (column-constraints column)))

(defun table-indices (table-class)
  "Extract a list of index objects from a table."
  (remove-if #'null
             (mapcar #'(lambda (column)
                         (when (column-index-p column)
                           (make-instance 'index
                                          :column (symbol-to-sql (column-name column)))))
                     (table-columns table-class))))

(defun table-column-definitions (table-class database)
  "Extract a list of column definitions from a table.

The database parameter is used to convert the type objects to SQL strings."
  (declare (type table-class table-class)
           (type database database))
  (mapcar #'(lambda (column)
              (make-instance 'column-definition
                             :name (symbol-to-sql (column-name column))
                             :type (type-sql (column-type column)
                                             database)))
          (table-columns table-class)))

;;; Create table definitions

(defun make-table-definition (table-class database)
  "Create a @c(table-definition) object given a @c(table-class) instance and the
database where the definition will be applied."
  (declare (type table-class table-class)
           (type database database))
  (make-instance 'table-definition
                 :name (table-name table-class)
                 :columns (table-column-definitions table-class database)
                 :constraints (table-constraints table-class)
                 :indices (table-indices table-class)))

;; Jesus Christ this is horrible
(defparameter +create-table-format+
  "CREATE TABLE ~A (
~{  ~A~^,
~}~{,
  ~A~}
)"
  "The format string to create tables.")

(defun constraint-name (constraint constraints)
  "Return the constraint's string name."
  (format nil "\"~A_~D\""
          (constraint-partial-name constraint)
          (position constraint constraints :test #'eq)))

(defun index-name (index indices)
  "Return the index's string name."
  (format nil "\"index_~D\"" (position index indices :test #'eq)))

(defun table-definition-sql (table-definition)
  "Return a list of SQL statements needed to create a table from its definition."
  (with-slots (name columns constraints indices) table-definition
    (append
     (list
      (format nil +create-table-format+
              name
              (mapcar #'(lambda (column)
                          (with-slots (name type) column
                            (format nil "~A ~A" name type)))
                      columns)
              (mapcar #'(lambda (constraint)
                          (render-constraint constraint
                                             (constraint-name constraint constraints)))
                      constraints)))
     (mapcar #'(lambda (index)
                 (add-index index (index-name index indices) name))
             indices))))

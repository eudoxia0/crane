(in-package :cl-user)
(defpackage crane.table.serialize
  (:use :cl)
  (:import-from :crane.serialize
                :to-plist
                :from-plist
                :serialize
                :deserialize)
  (:import-from :crane.types
                :sql-type
                :varchar
                :varchar-length)
  (:import-from :crane.table
                :column
                :column-name
                :column-type
                :column-null-p
                :column-unique-p
                :column-primary-p
                :column-index-p
                :column-foreign
                :column-autoincrement-p
                :foreign-key
                :foreign-key-table
                :foreign-key-on-delete
                :foreign-key-on-update
                :table-class
                :table-name
                :table-abstract-p
                :table-columns)
  (:export :storable-table)
  (:documentation "Serialize tables to S-expressions and back into tables."))
(in-package :crane.table.serialize)

;;; Serializing

;; Columns

(defmethod to-plist ((object column))
  (append (list :name (column-name object)
                :type (serialize (column-type object))
                :nullp (column-null-p object)
                :uniquep (column-unique-p object)
                :primaryp (column-primary-p object)
                :indexp (column-index-p object))
          (if (slot-boundp object 'column-foreign)
              (list :foreign (serialize (column-foreign object)))
              nil)
          (list :autoincrementp (column-autoincrement-p object))))

;; Types

(defmethod to-plist ((object varchar))
  (list :length (varchar-length object)))

;; Foreign key relations

(defmethod to-plist ((object foreign-key))
  "Encode a foreign key relation."
  (list :foreign-table (foreign-key-table object)
        :on-delete (foreign-key-on-delete object)
        :on-update (foreign-key-on-update object)))

;; Tables

(defmethod to-plist ((object table-class))
  "Encode a table."
  (list :name (table-name object)
        :abstractp (table-abstract-p object)
        :columns (mapcar #'serialize (table-columns object))))

;;; Deserializing

;; Columns

(defmethod from-plist ((class (eql 'column)) plist)
  "Restore a column."
  (let ((instance (make-instance 'column
                                 :name (getf plist :name)
                                 :col-type (deserialize (getf plist :type))
                                 :nullp (getf plist :nullp)
                                 :uniquep (getf plist :uniquep)
                                 :primaryp (getf plist :primaryp)
                                 :indexp (getf plist :indexp)
                                 :autoincrementp (getf plist :autoincrementp))))
    (let ((foreign (getf plist :foreign)))
      (when foreign
        (setf (slot-value instance 'column-foreign) foreign)))
    instance))

;; Types

(defmethod from-plist ((class (eql 'varchar)) plist)
  (make-instance 'varchar :length (getf plist :length)))

;; Foreign keys

(defmethod from-plist ((class (eql 'foreign-key)) plist)
  (make-instance 'foreign-key
                 :foreign-table (getf plist :foreign-table)
                 :on-delete (getf plist :on-delete)
                 :on-update (getf plist :on-update)))

;; Tables

(defclass storable-table ()
  ((name :reader table-name
         :initarg :name
         :type symbol)
   (abstract :reader table-abstract-p
             :initarg :abstractp
             :type boolean)
   (columns :reader table-columns
            :initarg :columns
            :type list))
  (:documentation "Since @c(table-class) objects are not really amenable to
  deserialization, we deserialize them into this class. This is the class we use
  for diffing."))

(defmethod from-plist ((class (eql 'table-class)) plist)
  (make-instance 'storable-table
                 :name (getf plist :name)
                 :abstractp (getf plist :abstractp)
                 :columns (mapcar #'deserialize (getf plist :columns))))

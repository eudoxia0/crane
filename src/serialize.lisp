(in-package :cl-user)
(defpackage crane.serialize
  (:use :cl)
  (:import-from :yason
                :encode-slots
                :encode-object
                :with-object
                :encode-object-elements
                :with-object-element)
  (:import-from :crane.types
                :sql-type
                :varchar
                :varchar-length)
  (:import-from :crane.table
                :column
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
                :referential-action-name)
  (:documentation "Tools for serializing and de-serializing various internal
  data structures."))
(in-package :crane.serialize)

;;; Utilities

(defun json-boolean (boolean)
  "Convert a boolean to a JSON boolean."
  (if boolean
      'yason:true
      'yason:false))

(defun encode-symbol (symbol)
  "Encode a symbol into a string."
  (concatenate 'string
               (uiop:symbol-package-name symbol)
               "::"
               symbol))

;;; Serializing columns

(defmethod encode-slots progn ((column column))
  (with-object-element ("type")
    (with-object ()
      (let ((type (column-type column)))
        (encode-object-elements
         "name" (encode-symbol (class-name type)))
        (with-object-element ("slots")
          (encode-object type)))))
  (encode-object-elements
   "nullp" (json-boolean (column-null-p column))
   "uniquep" (json-boolean (column-unique-p column))
   "primaryp" (json-boolean (column-primary-p column))
   "indexp" (json-boolean (column-index-p column))
   (when (slot-boundp column 'column-foreign)
     (with-object-element ("foreign")
       (encode-object (column-foreign column))))
   (encode-object-elements
    "autoincrementp" (json-boolean (column-autoincrement-p column)))))

;;; Serializing types

(defmethod encode-slots progn ((type sql-type))
  "The default method for an SQL type. Does nothing."
  (declare (ignore type))
  t)

(defmethod encode-slots progn ((type varchar))
  "Encode the slots of the varchar type."
  (encode-object-elements
   "length" (varchar-length type)))

;;; Serializing foreign key relations

(defmethod encode-slots progn ((foreign foreign-key))
  "Encode the slots of a foreign key relation."
  (encode-object-elements
   "foreign_table" (encode-symbol (foreign-key-table foreign))
   "on_delete" (referential-action-name
                (foreign-key-on-delete foreign))
   "on_update" (referential-action-name
                (foreign-key-on-update foreign))))


;;; Serializing Tables

;;; Reconstructing tables

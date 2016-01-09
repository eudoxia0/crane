(in-package :cl-user)
(defpackage crane.serialize
  (:use :cl)
  (:export :to-plist
           :from-plist
           :serialize
           :deserialize)
  (:documentation "An interface to serialize/deserialize internal data
  structures. This package is used by the migrations system to record the
  definition of tables, which requires serializing the table, column, and type
  objects used in the table definition."))
(in-package :crane.serialize)

(defgeneric to-plist (object)
  (:documentation "Serialize a CLOS object to a property list.")

  (:method ((type t))
    "The default method: return an empty list."
    nil))

(defgeneric from-plist (class plist)
  (:documentation "Create a CLOS instance from a plist and the class name. Use
  @cl:spec(eql) specialization for the @cl:param(class) argument.")

  (:method ((class symbol) plist)
    "The default method: just create the instance."
    (make-instance class)))

(defun serialize (object)
  "Serialize an object."
  (list :class (class-name (class-of object))
        :data (to-plist object)))

(defun deserialize (plist)
  "Deserialize an object."
  (let ((class (getf plist :class))
        (data (getf plist :data)))
    (from-plist class data)))

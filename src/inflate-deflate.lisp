;;;; Inflation/deflation map CLOS objects to SQL strings and vice-versa. This is
;;;; unrelated to the ORM, and meant to allow complex column datatypes to be
;;;; mapped to CLOS objects. For example, mapping SQL timestamps to structures
;;;; that represent time, or mapping other more complex SQL types to CLOS
;;;; objects.
(in-package :cl-user)
(defpackage :crane.inflate-deflate
  (:use :cl :anaphora)
  (:export :inflate
           :definflate))
(in-package :crane.inflate-deflate)

(defgeneric inflate (obj)
  (:documentation "Turn a CLOS object into a string for insertion in the database."))

(defmacro definflate ((obj-name obj-type) &rest body)
  `(defmethod crane.inflate-deflate:inflate ((,obj-name ,obj-type))
     ,@body))

(definflate (str string) str)
(definflate (num number) num)

(defgeneric deflate (obj type-name)
  (:documentation "Turn a string into a CLOS object."))

(defmacro defdeflate ((obj-name obj-type-name) &rest body)
  `(defmethod crane.inflate-deflate:deflate ((,obj-name string)
                                             (type (eql ,obj-type-name)))
     ,@body))

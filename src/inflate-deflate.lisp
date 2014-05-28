;;;; Inflation/deflation map SQL string to CLOS objects. This is unrelated to
;;;; the ORM, and meant to allow complex column datatypes to be mapped to CLOS
;;;; objects. For example, mapping SQL timestamps to structures that represent
;;;; time, or mapping other more complex SQL types to CLOS objects.
(in-package :cl-user)
(defpackage :crane.inflate-deflate
  (:use :cl :anaphora)
  (:export :inflate
           :definflate
           :deflate
           :defdeflate))
(in-package :crane.inflate-deflate)

(defgeneric deflate (obj)
  (:documentation "Turn a CLOS object into a string for insertion in the database."))

(defmacro defdeflate ((obj-name obj-type) &rest body)
  `(defmethod crane.inflate-deflate:inflate ((,obj-name ,obj-type))
     ,@body))

(defdeflate (str string) str)
(defdeflate (num number) num)

(defgeneric inflate (obj type-name)
  (:documentation "Turn a string into a CLOS object."))

(defmacro definflate ((obj-name obj-type-name) &rest body)
  `(defmethod crane.inflate-deflate:inflate (,obj-name
                                             (type (eql ,obj-type-name)))
     ,@body))

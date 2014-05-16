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

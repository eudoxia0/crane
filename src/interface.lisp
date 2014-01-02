;;;; This file contains the methods used to access and alter database records in
;;;; an object-oriented way.

(in-package :crane)
(annot:enable-annot-syntax)

@export
(defmethod filter ((class table-class) &rest params)
  
  )

@export
(defmethod filter ((class-name symbol) &rest params)
  (apply #'filter (cons (find-class class-name) params)))

;;;; This file contains the methods used to access and alter database records in
;;;; an object-oriented way.

(in-package :crane)
(annot:enable-annot-syntax)

(defmethod slot-tuple (object)
  (mapcar #'symbol-name
          (remove-if-not #'(lambda (slot)
                             (slot-boundp object slot))
                         (mapcar #'closer-mop:slot-definition-name
                                 (closer-mop:class-slots (class-of object))))))

@doc "Transform an object into a tuple of its values. Useful for INSERT
statements."
(defmethod tuple (object)
  (mapcar #'(lambda (slot)
              (slot-value object slot))
          (remove-if-not #'(lambda (slot)
                             (slot-boundp object slot))
                         (mapcar #'closer-mop:slot-definition-name
                                 (closer-mop:class-slots (class-of object))))))

@export
(defmethod save (object)
  )

@export
(defmethod filter ((class table-class) &rest params)
  
  )

@export
(defmethod filter ((class-name symbol) &rest params)
  (apply #'filter (cons (find-class class-name) params)))

;;;; This file contains the methods used to access and alter database records in
;;;; an object-oriented way.

(in-package :crane)
(annot:enable-annot-syntax)

(defmethod drop-table ((table table-class))
  (execute
   (prepare (concatenate 'string
                         "DROP TABLE "
                         (crane.sql:sqlize (table-name table)))
            (db table))))

@export
(defmethod drop-table ((table-name symbol))
  (drop-table (find-class table-name)))

(defmethod slot-tuple ((obj <table>))
  (mapcar #'symbol-name
          (remove-if-not #'(lambda (slot)
                             (slot-boundp obj slot))
                         (mapcar #'closer-mop:slot-definition-name
                                 (closer-mop:class-slots (class-of obj))))))

@doc "Transform an object into a tuple of its values. Useful for INSERT
statements."
(defmethod tuple ((obj <table>))
  (mapcar #'(lambda (slot)
              (slot-value object slot))
          (remove-if-not #'(lambda (slot)
                             (slot-boundp object slot))
                         (mapcar #'closer-mop:slot-definition-name
                                 (closer-mop:class-slots (class-of object))))))

@export
(defmethod save ((obj <table>))
  (pprint (slot-tuple obj))
  (pprint "Saving"))

@export
(defmethod filter ((class table-class) &rest params)
  
  )

@export
(defmethod filter ((class-name symbol) &rest params)
  (apply #'filter (cons (find-class class-name) params)))

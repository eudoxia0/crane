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

(defmethod slot-tuple (obj)
  (remove-if-not #'(lambda (slot)
                     (slot-boundp obj slot))
                 (mapcar #'closer-mop:slot-definition-name
                         (closer-mop:class-slots (class-of obj)))))

@doc "Transform an object into a call to the set= function used by SxQL."
(defun make-set (obj)
  (let ((slot-names (slot-tuple obj)))
    (iter (for slot in slot-names)
      (appending (list (intern (crane.sql:sqlize (symbol-name slot))
                               :keyword)
                       (slot-value obj slot))))))

(defmethod initialize-instance :after ((obj <table>) &key)
  (query (sxql:insert-into
             (table-name (class-of obj))
           (apply #'sxql.clause:make-clause
                  (cons :set=
                        (make-set obj))))
      (db (class-of obj))))
     
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

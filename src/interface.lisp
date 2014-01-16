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
  (let ((set (make-set obj)))
    (query (sxql:update (table-name (class-of obj))
                        (apply #'sxql.clause:make-clause
                               (cons :set= set))
                        (sxql:where (:= :id (getf set :|id|))))
        (db (class-of obj)))))

@export
(defmethod del ((obj <table>))
  (query (sxql:delete-from (table-name (class-of obj))
           (sxql:where (:= :id (getf (make-set obj) :|id|))))
      (db (class-of obj))))

(defparameter *sxql-operators*
  (list :not :is-null :not-null :desc :asc :distinct :include :constructor :type :=
        :!= :< :> :<= :>= :as :in :not-in :like :or :and :+ :- :sql-op-name :* :/ :%
        :union :union-all :include :constructor))

(defun sqlize-all (tree)
  "This is a load-bearing hack, until I overhaul everything in src/sql.lisp to
SxQL."
  (cond ((null tree)
         nil)
        ((atom tree)
         (if (and (keywordp tree)
                  (not (member tree *sxql-operators*)))
             (intern (crane.sql:sqlize tree)
                     :keyword)
             tree))
        (t
         (mapcan #'sqlize-all tree))))

@export
(defmacro filter (class &rest params)
  (let ((equal-params (remove-if-not #'keywordp params))
        (fn-params
          (remove-if #'keywordp
                     (iter (for item in params)
                       (for prev previous item back 1 initially nil)
                       (unless (keywordp prev) (collect item))))))
    `(crane:query
         ,(append
           `(sxql:select :*
              (sxql:from (table-name (find-class ',class))))
           (when params
             `((sxql:where (:and ,@(mapcar #'(lambda (slot-name)
                                               (list :=
                                                     (intern (crane.sql:sqlize slot-name)
                                                             :keyword)
                                                     (getf params slot-name)))
                                           equal-params)
                                 ,@(sqlize-all fn-params)))))))))

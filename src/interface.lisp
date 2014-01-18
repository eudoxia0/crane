;;;; This file contains the methods used to access and alter database records in
;;;; an object-oriented way.

(defpackage :crane.interface
  (:use :cl :anaphora :cl-annot.doc :iter)
  (:import-from :crane.meta
                :table-class
                :table-name
                :db
                :col-foreign)
  (:import-from :crane.sql
                :sqlize
                :sqlize-all)
  (:import-from :crane.query
                :query))
(in-package :crane.interface)
(annot:enable-annot-syntax)

(defmethod drop-table ((table table-class))
  (execute
   (prepare (concatenate 'string
                         "DROP TABLE "
                         (sqlize (table-name table)))
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
      (appending (list (intern (sqlize (symbol-name slot))
                               :keyword)
                       ;; TODO: If the slot is a foreign key, and is storing an
                       ;; instance of an object, store that object's id
                       (slot-value obj slot))))))

(defmethod create% ((obj crane.table:<table>))
  (query (sxql:insert-into
             (table-name (class-of obj))
           (apply #'sxql.clause:make-clause
                  (cons :set=
                        (make-set obj))))
      (db (class-of obj)))
  obj)

@export
(defmacro create (class-name &rest args)
  `(crane.interface::create% (make-instance ,class-name ,@args)))
     
@export
(defmethod save ((obj crane.table:<table>))
  (let ((set (make-set obj)))
    (query (sxql:update (table-name (class-of obj))
                        (apply #'sxql.clause:make-clause
                               (cons :set= set))
                        (sxql:where (:= :id (getf set :|id|))))
        (db (class-of obj)))))

@export
(defmethod del ((obj crane.table:<table>))
  (query (sxql:delete-from (table-name (class-of obj))
           (sxql:where (:= :id (getf (make-set obj) :|id|))))
      (db (class-of obj))))


(defun clean-tuple (tuple)
  (flet ((process-key (key)
           (intern (string-upcase
                    (map 'string #'(lambda (char) (if (eql char #\_) #\- char))
                         (symbol-name key)))
                   :keyword)))
    (iter (for (key value) on tuple by #'cddr)
          (appending
           (list (process-key key) value)))))

(defmethod tuple->object ((class-name symbol) tuple)
  (apply #'make-instance (cons class-name (clean-tuple tuple))))


@export
(defmacro filter (class &rest params)
  (let* ((equal-params (remove-if-not #'keywordp params))
         (fn-params
           (remove-if #'keywordp
                      (iter (for item in params)
                        (for prev previous item back 1 initially nil)
                        (unless (keywordp prev) (collect item))))))
    `(mapcar #'(lambda (tuple) (tuple->object ,class tuple))
             (crane.query:query
                 ,(append
                   `(sxql:select :*
                      (sxql:from (table-name (find-class ,class))))
                   (when params
                     `((sxql:where (:and ,@(mapcar #'(lambda (slot-name)
                                                       (list :=
                                                             (intern (sqlize slot-name)
                                                                     :keyword)
                                                             (getf params slot-name)))
                                                   equal-params)
                                         ,@(sqlize-all fn-params))))))))))

@export
(defmacro single (class &rest params)
  `(first (filter ,class ,@params)))

@export
(defmacro single! (class &rest params)
  `(anaphora:aif (get ,class ,@params)
                 anaphora:it
                 (error 'crane.errors:query-error
                        :text "Call to get returned more than one result.")))

@export
(defmacro single-or-create (class &rest params)
  `(anaphora:aif (get ,class ,@params)
                 anaphora:it
                 (create ,class ,@params)))

(defun find-slot (obj name)
  (aif (remove-if-not #'(lambda (slot-name)
                          (eql name
                               (closer-mop:slot-definition-name slot-name)))
                      (closer-mop:class-slots (class-of obj)))
       (first it)))

@export
(defmacro deref (obj field)
  `(filter
    (first (crane.meta:col-foreign (crane.interface::find-slot
                                    ,obj ,field)))
    :id (slot-value ,obj (closer-mop:slot-definition-name
                          (crane.interface::find-slot ,obj ',(intern "ID" *package*))))))

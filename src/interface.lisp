;;;; This file contains the methods used to access and alter database records in
;;;; an object-oriented way.

(defpackage :crane.interface
  (:use :cl :anaphora :cl-annot.doc :iter)
  (:import-from :crane.utils
                :make-keyword)
  (:import-from :crane.meta
                :table-class
                :table-name
                :db
                :col-foreign)
  (:import-from :crane.sql
                :sqlize)
  (:import-from :crane.query
                :prepare
                :execute
                :query))
(in-package :crane.interface)
(annot:enable-annot-syntax)

(defmethod drop-table ((table table-class))
  (execute
   (prepare (sxql:drop-table (table-name table))
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
                       ;; TODO: Inflate Lisp value to database
                       (slot-value obj slot))))))

@export
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


@doc "Process a tuple created by a CL-DBI into a format that can be accepted by
make-instance."
(defun clean-tuple (tuple)
  (flet ((process-key (key)
           (intern (string-upcase
                    (map 'string #'(lambda (char) (if (eql char #\_) #\- char))
                         (symbol-name key)))
                   :keyword)))
    (iter (for (key value) on tuple by #'cddr)
          (appending
           ;; TODO: Deflate CL-DBI value to database
           (list (process-key key) value)))))

@doc "Convert a tuple produced by CL-DBI to a CLOS instance."
(defmethod tuple->object ((table table-class) tuple)
  (apply #'make-instance (cons table (clean-tuple tuple))))

(defmethod tuple->object ((table-name symbol) tuple)
  (tuple->object (find-class table-name) tuple))

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
                                                             (make-keyword slot-name)
                                                             (getf params slot-name)))
                                                   equal-params)
                                         ,@fn-params)))))))))

@export
(defmacro exists (class &rest params)
  `(if (filter ,class ,@params) t nil))

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
  `(single
    (first (crane.meta:col-foreign (crane.interface::find-slot
                                    ,obj ,field)))
    :id (slot-value ,obj (closer-mop:slot-definition-name
                          (crane.interface::find-slot ,obj ',(intern "ID" *package*))))))

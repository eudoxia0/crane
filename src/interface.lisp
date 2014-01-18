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
                       ;; TODO: If the slot is a foreign key, and is storing an
                       ;; instance of an object, store that object's id
                       (slot-value obj slot))))))

(defmethod create% ((obj <table>))
  (query (sxql:insert-into
             (table-name (class-of obj))
           (apply #'sxql.clause:make-clause
                  (cons :set=
                        (make-set obj))))
      (db (class-of obj)))
  obj)

@export
(defmacro create (class-name &rest args)
  `(crane::create% (make-instance ,class-name ,@args)))
     
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
             (crane:query
                 ,(append
                   `(sxql:select :*
                      (sxql:from (table-name (find-class ,class))))
                   (when params
                     `((sxql:where (:and ,@(mapcar #'(lambda (slot-name)
                                                       (list :=
                                                             (intern (crane.sql:sqlize slot-name)
                                                                     :keyword)
                                                             (getf params slot-name)))
                                                   equal-params)
                                         ,@(crane.sql:sqlize-all fn-params))))))))))

@export
(defmethod deref ((obj <table>) (field symbol))
  (let ((slot (remove-if-not #'(lambda (slot-name)
                                 (eql slot-name
                                      (closer-mop:slot-definition-name slot-name)))
                        (closer-mop:class-slots (class-of obj)))))
    (filter (first (col-foreign slot)) :id (slot-value obj field))))

(in-package :cl-user)
(defpackage :crane.interface
  (:use :cl :anaphora :cl-annot.doc :iter)
  (:import-from :crane.utils
                :make-keyword
                :get-class-slot)
  (:import-from :crane.meta
                :table-class
                :table-name
                :db
                :col-foreign)
  (:import-from :crane.sql
                :sqlize)
  (:import-from :crane.query
                :query)
  (:import-from :crane.inflate-deflate
                :deflate
                :inflate)
  (:export :drop-table
           :create%
           :create
           :create-from-plist
           :save
           :del
           :plist->object
           :filter
           :do-filter
           :exists
           :single
           :single!
           :single-or-create
           :deref)
  (:documentation "This package contains the methods used to access and alter
 database records in an object-oriented way."))
(in-package :crane.interface)
(annot:enable-annot-syntax)

(defmethod drop-table ((table table-class))
  (query (sxql:drop-table (table-name table))
         (db table)))

(defmethod drop-table ((table-name symbol))
  (drop-table (find-class table-name)))

(defmethod slot-tuple (obj)
  (remove-if-not #'(lambda (slot)
                     (slot-boundp obj slot))
                 (mapcar #'closer-mop:slot-definition-name
                         (closer-mop:class-slots (class-of obj)))))

@doc "Transform an object into a call to the set= function used by
SxQL. Deflation happens here."
(defun make-set (obj)
  (let ((slot-names (slot-tuple obj)))
    (iter (for slot in slot-names)
      (appending
       (if (slot-boundp obj slot)
           (list (make-keyword slot)
                 ;; TODO: If the slot is a foreign key, and is storing an
                 ;; instance of an object, store that object's id
                 (deflate (slot-value obj slot))))))))

(defmacro create% (obj)
  `(let* ((obj ,obj)
          (class (class-of obj))
          (table-name (table-name class))
          (db-type
            (crane.connect:database-type
             (crane.connect:get-db (db class))))
          (results
            (if (eq db-type :sqlite3)
                (query (sxql:insert-into
                        table-name
                        (apply #'sxql.clause:make-clause
                               (cons :set=
                                     (make-set obj))))
                       (db (class-of obj)))
                (query (sxql:insert-into
                        table-name
                        (apply #'sxql.clause:make-clause
                               (cons :set=
                                     (make-set obj)))
                        (sxql:make-op :returning :id))
                       (db (class-of obj)))))
          (id
            (if (eq db-type :sqlite3)
                (let ((res (let ((sxql:*QUOTE-CHARACTER* nil))
                             (query (sxql:select :|last_insert_rowid()|)))))
                  (getf (first res) :|last_insert_rowid()|))
                (getf (first results) :|id|))))
     ;; Query its ID
     (setf (slot-value obj (intern "ID" (symbol-package (class-name class))))
           id)
     obj))

(defmacro create (class-name &rest args)
  `(crane.interface::create% (make-instance ,class-name ,@args)))

(defmacro create-from-plist (class plist)
  `(crane.interface::create% (apply #'make-instance ,class ,plist)))

(defmethod save ((obj crane.table:<table>))
  (let ((set (make-set obj)))
    (query (sxql:update (table-name (class-of obj))
                        (apply #'sxql.clause:make-clause
                               (cons :set= set))
                        (sxql:where (:= :id (getf set :id))))
        (db (class-of obj)))))

(defmethod del ((obj crane.table:<table>))
  (query (sxql:delete-from (table-name (class-of obj))
           (sxql:where (:= :id (getf (make-set obj) :id))))
      (db (class-of obj))))


@doc "Process a plist returned by CL-DBI into a format that can be accepted by
make-instance. Inflation happens here."
(defmethod clean-tuple ((table table-class) tuple)
  (flet ((process-key (key)
           (intern (string-upcase (symbol-name key))
                   :keyword)))
    (iter (for (key value) on tuple by #'cddr)
          (appending
           (let* ((processed-key (process-key key))
                  (slot (get-class-slot table processed-key))
                  (type (crane.meta:col-type slot)))
             (list processed-key (inflate value type)))))))

@doc "Convert a tuple produced by CL-DBI to a CLOS instance."
(defmethod plist->object ((table table-class) tuple)
  (apply #'make-instance (cons table (clean-tuple table tuple))))

(defmethod plist->object ((table-name symbol) tuple)
  (plist->object (find-class table-name) tuple))

(defmacro filter (class &rest params)
  (let* ((equal-params (remove-if-not #'keywordp params))
         (fn-params
           (remove-if #'keywordp
                      (iter (for item in params)
                        (for prev previous item back 1 initially nil)
                        (unless (keywordp prev) (collect item))))))
    `(mapcar #'(lambda (tuple) (plist->object ,class tuple))
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
                                         ,@fn-params)))))
                 (crane.meta:db ,class)))))

(defmacro do-filter ((result-name class &rest params) &rest body)
  (let* ((equal-params (remove-if-not #'keywordp params))
         (fn-params
           (remove-if #'keywordp
                      (iter (for item in params)
                        (for prev previous item back 1 initially nil)
                        (unless (keywordp prev) (collect item))))))
    `(crane.query:do-query
         (,result-name
          ,(append
            `(sxql:select :*
               (sxql:from (table-name (find-class ,class))))
            (when params
              `((sxql:where (:and ,@(mapcar #'(lambda (slot-name)
                                                (list :=
                                                      (make-keyword slot-name)
                                                      (getf params slot-name)))
                                            equal-params)
                                  ,@fn-params)))))
          (crane.meta:db ,class))
         (let ((,result-name (plist->object ,class ,result-name)))
           ,@body))))

(defmacro exists (class &rest params)
  `(if (filter ,class ,@params) t nil))

(defmacro single (class &rest params)
  `(first (filter ,class ,@params)))

(defmacro single! (class &rest params)
  `(anaphora:aif (get ,class ,@params)
                 anaphora:it
                 (error 'crane.errors:query-error
                        :text "Call to get returned more than one result.")))

(defmacro single-or-create (class &rest params)
  `(anaphora:aif (get ,class ,@params)
                 anaphora:it
                 (create ,class ,@params)))

(defmacro deref (obj field)
  `(single
    (first (crane.meta:col-foreign (crane.utils::find-slot
                                    ,obj ,field)))
    :id (slot-value ,obj (closer-mop:slot-definition-name
                          (crane.utils::get-slot ,obj :id)))))

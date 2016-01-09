(in-package :cl-user)
(defpackage crane.table
  (:use :cl)
  (:import-from :closer-mop
                :standard-class
                :standard-direct-slot-definition
                :standard-effective-slot-definition
                :compute-effective-slot-definition)
  ;; Foreign key relations
  (:export :foreign-key
           :foreign-key-table
           :foreign-key-on-delete
           :foreign-key-on-update
           :referential-action-name)
  ;; Tables
  (:export :table-class
           :table-name
           :table-abstract-p
           :table-columns)
  ;; Columns
  (:export :column
           :column-name
           :column-type
           :column-null-p
           :column-unique-p
           :column-primary-p
           :column-index-p
           :column-foreign
           :column-autoincrement-p)
  ;; Deftable
  (:export :standard-db-object
           :deftable
           :id
           :slot-type)
  (:documentation "This package defines the metaclasses that map CLOS objects to
  SQL tables, and some basic operations on them."))
(in-package :crane.table)

;;; Foreign key relations

(defvar +referential-actions+
  (list :cascade "CASCADE"
        :restrict "RESTRICT"
        :no-action "NO ACTION"
        :set-null "SET NULL"
        :set-default "SET DEFAULT")
  "A map of allowed referential actions to their SQL names.")

(defun referential-action-name (action)
  "Find the SQL string of a referential action."
  (getf +referential-actions+ action))

(defun ensure-valid-referential-action (action)
  "Ensure a referential action is valid."
  (or (referential-action-name action)
      (error "No such referential action: ~A." action)))

(defclass foreign-key ()
  ((table :reader foreign-key-table
          :initarg :table
          :type symbol
          :documentation "The name of the table referenced by this relation.")
   (on-delete :reader foreign-key-on-delete
              :initarg :on-delete
              :initform :no-action
              :type keyword
              :documentation "The action to take on deletion.")
   (on-update :reader foreign-key-on-update
              :initarg :on-update
              :initform :no-action
              :type keyword
              :documentation "The action to take on updates."))
  (:documentation "A foreign key relationship."))

(defmethod initialize-instance :after ((foreign foreign-key) &key)
  "Verify that the referential actions are allowed."
  (ensure-valid-referential-action (foreign-key-on-delete foreign))
  (ensure-valid-referential-action (foreign-key-on-update foreign)))

(defun make-foreign-key (foreign-table-name &key (on-delete :no-action)
                                              (on-update :no-action))
  "Create a foreign key object."
  (make-instance 'foreign-key
                 :table foreign-table-name
                 :on-delete on-delete
                 :on-update on-update))

;;; Tables

(defclass table-class (standard-class)
  ((abstractp :reader table-abstract-p
              :initarg :abstractp
              :initform nil
              :type boolean
              :documentation "Whether the class corresponds to an SQL table or not."))
  (:documentation "A table metaclass."))

(defmethod table-name ((class table-class))
  "Return the SQL name of the table, a string."
  (crane.util:symbol-to-sql (class-name class)))

(defmethod table-columns ((class table-class))
  "Return a list of column objects."
  (closer-mop:class-slots class))

;;; Columns

(defclass table-class-direct-slot-definition (standard-direct-slot-definition)
  ((direct-slot-type :reader direct-slot-type
                     :initarg :col-type)
   (direct-slot-null-p :reader direct-slot-null-p
                       :initarg :nullp
                       :initform t
                       :type boolean)
   (direct-slot-unique-p :reader direct-slot-unique-p
                         :initarg :uniquep
                         :initform nil
                         :type boolean)
   (direct-slot-primary-p :reader direct-slot-primary-p
                          :initarg :primaryp
                          :initform nil
                          :type boolean)
   (direct-slot-index-p :reader direct-slot-index-p
                        :initarg :indexp
                        :initform nil
                        :type boolean)
   (direct-slot-foreign :reader direct-slot-foreign
                        :initarg :foreign
                        :type list)
   (direct-slot-autoincrement-p :reader direct-slot-autoincrement-p
                                :initarg :autoincrementp
                                :initform nil
                                :type boolean))
  (:documentation "The direct slot definition class of table slots. The
  @c(:initargs) here are what the user sees: they are the names of slot options
  passed to the class. The values stored in these slots are the forms the used
  provides on class definition."))

(defclass column (standard-effective-slot-definition)
  ((column-type :reader column-type
                :initarg :col-type
                :type crane.types:sql-type
                :documentation "The type of the column.")
   (column-null-p :reader column-null-p
                  :initarg :nullp
                  :type boolean
                  :documentation "Whether the column is nullable.")
   (column-unique-p :reader column-unique-p
                    :initarg :uniquep
                    :type boolean
                    :documentation "Whether the column is unique.")
   (column-primary-p :reader column-primary-p
                     :initarg :primaryp
                     :type boolean
                     :documentation "Whether the column is a primary key.")
   (column-index-p :reader column-index-p
                   :initarg :indexp
                   :type boolean
                   :documentation "Whether the column is an index in the database.")
   (column-foreign :reader column-foreign
                   :initarg :foreign
                   :type foreign-key
                   :documentation "Describes a foreign key relationship.")
   (column-autoincrement-p :reader column-autoincrement-p
                           :initarg :autoincrementp
                           :type boolean
                           :documentation "Whether the column should be
                           autoincremented."))
  (:documentation "A slot of a table class. This is what the slots look like in
  the end: the values stored in the slots are computed from the forms in the
  direct slot definition by the @c(compute-effective-slot-definition) method."))

(defmethod column-name ((column column))
  "Return the name of the column, a symbol."
  (closer-mop:slot-definition-name column))

(defun type-specifier-to-instance (specifier)
  (if (listp specifier)
      (apply #'make-instance specifier)
      (make-instance specifier)))

(defmethod compute-effective-slot-definition ((class table-class)
                                              slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((direct-slot (first direct-slot-definitions))
        (column (call-next-method)))
    (setf (slot-value column 'column-type)
          (type-specifier-to-instance (direct-slot-type direct-slot))

          (slot-value column 'column-null-p)
          (direct-slot-null-p direct-slot)

          (slot-value column 'column-unique-p)
          (direct-slot-unique-p direct-slot)

          (slot-value column 'column-primary-p)
          (direct-slot-primary-p direct-slot)

          (slot-value column 'column-index-p)
          (direct-slot-index-p direct-slot)

          (slot-value column 'column-autoincrement-p)
          (direct-slot-autoincrement-p direct-slot))

    ;; If the user supplied a foreign key, create the foreign key object and
    ;; assign it to the column
    (when (slot-boundp direct-slot 'direct-slot-foreign)
      (setf (slot-value column 'column-foreign)
            (apply #'make-foreign-key
                   (direct-slot-foreign direct-slot))))

    column))

;;; Assorted MOPery

(defmethod closer-mop:validate-superclass ((class table-class) (super standard-class))
  "A table class cannot be a subclass of a standard-class."
  t)

(defmethod closer-mop:validate-superclass ((class standard-class)
                                           (super table-class))
  "A standard-class can be the subclass of a table-class."
  t)

(defmethod closer-mop:direct-slot-definition-class ((class table-class)
                                                    &rest initargs)
  (declare (ignore class initargs))
  (find-class 'table-class-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class table-class)
                                                       &rest initargs)
  (declare (ignore class initargs))
  (find-class 'column))

(defgeneric slot-type (table-name slot-name)
  (:documentation "Given the name of a slot, return its type instance."))

;;; The deftable macro

(defclass standard-db-object ()
  ()
  (:metaclass table-class)
  (:documentation "The base class of all database objects."))

(defmacro deftable (name (&rest superclasses) slots &body options)
  "Define a table."
  (flet ((process-slot (plist)
           (let ((options (alexandria:remove-from-plist plist :type :initform)))
             (append options
                     (if (getf plist :type)
                         (list :col-type (getf plist :type))
                         (error "No :type option in slot definition."))
                     (when (getf plist :initform)
                       (list :initform (getf plist :initform)))))))
    `(progn
       (defclass ,name ,(if superclasses superclasses `(standard-db-object))
         ,(append
           `((id ,@(process-slot '(:initarg :id
                                   :type crane.types:column-id
                                   :primaryp t))))
           (mapcar #'(lambda (slot)
                       (cons (first slot)
                             (process-slot (rest slot))))
                   slots))
         ,@options
         (:metaclass table-class))
       (closer-mop:finalize-inheritance (find-class ',name))

       (defmethod slot-type ((table-name (eql ',name)) slot-name)
         (declare (type keyword slot-name))
         (case slot-name
           ,@(mapcar #'(lambda (slot)
                         `(,(first slot)
                           (type-specifier-to-instance ',(getf (rest slot) :type))))
              slots)))

       ',name)))

(defmethod id ((object standard-db-object))
  "Retrieve the object's ID."
  (slot-value object 'id))

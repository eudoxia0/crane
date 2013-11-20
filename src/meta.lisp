(in-package :crane)

(defclass table-class (closer-mop:standard-class)
  ((table-name :reader table-class-name :initarg :table-name)
   (abstract-p :reader table-class-abstract-p :initarg :abstract-p :initform nil)))

(defmethod table-name ((class table-class))
  (if (slot-boundp class 'table-name)
      (car (table-class-name class))
      (class-name class)))

(defmethod table-name ((class-name symbol))
  (table-name (find-class class-name)))

(defmethod abstract-p ((class table-class))
  (car (table-class-abstract-p class)))

(defmethod abstract-p ((class-name symbol))
  (abstract-p (find-class class-name)))

(defmethod closer-mop:validate-superclass ((class table-class) (super closer-mop:standard-class))
  t)

(defmethod closer-mop:validate-superclass ((class standard-class) (super table-class))
  t)

(defclass table-class-direct-slot-definition (closer-mop:standard-direct-slot-definition)
  ((col-type :initarg :col-type
             :reader  col-type)
   (col-null-p :initarg :col-null-p
             :reader  col-null-p
             :initform t)
   (col-unique-p :initarg :col-unique-p
             :reader  col-unique-p
             :initform nil)
   (col-index-p :initarg :col-index-p
             :reader  col-index-p
             :initform nil)
   (col-name :initarg :col-name
             :reader  col-name)
   (col-default :initarg :col-default
             :reader  col-default)))

(defclass table-class-effective-slot-definition (closer-mop:standard-effective-slot-definition)
  ((col-type :initarg :col-type
         :accessor col-type)
   (col-null-p :initarg :col-null-p
             :reader  col-null-p)
   (col-unique-p :initarg :col-unique-p
             :reader  col-unique-p)
   (col-index-p :initarg :col-index-p
             :reader  col-index-p)
   (col-name :initarg :col-name
             :reader  col-name)
   (col-default :initarg :col-default
             :reader  col-default)))

;;; Common Lisp is a vast ocean of possibilities, stretching infinitely
;;; and with no horizon... And here I am pretending to understand
;;; the MOP while trying not to end up in r/badcode, like a child
;;; playing in the surf...

(defmethod closer-mop:direct-slot-definition-class ((class table-class) &rest initargs)
  (declare (ignore class initargs))
  (find-class 'table-class-direct-slot-definition))

(defmethod closer-mop:effective-slot-definition-class ((class table-class) &rest initargs)
  (declare (ignore class initargs))
  (find-class 'table-class-effective-slot-definition))

(defmethod closer-mop:compute-effective-slot-definition ((class table-class) slot-name direct-slot-definitions)
  (declare (ignore slot-name))
  (let ((effective-slot-definition (call-next-method)))
    (setf (slot-value effective-slot-definition 'col-type)
          (col-type (first direct-slot-definitions))

          (slot-value effective-slot-definition 'col-null-p)
          (col-null-p (first direct-slot-definitions))
          
          (slot-value effective-slot-definition 'col-unique-p)
          (col-unique-p (first direct-slot-definitions))
          
          (slot-value effective-slot-definition 'col-index-p)
          (col-index-p (first direct-slot-definitions))
          
          (slot-value effective-slot-definition 'col-name)
          (if (slot-boundp (first direct-slot-definitions) 'col-name)
              (col-name (first direct-slot-definitions))
              slot-name)

          (slot-value effective-slot-definition 'col-default)
          (if (slot-boundp (first direct-slot-definitions) 'col-default)
              (col-default (first direct-slot-definitions))
              nil))
    effective-slot-definition))

(defmethod digest-slots ((class table-class))
  (loop for slot in (closer-mop:class-slots class) collecting
        (list :name (closer-mop:slot-definition-name slot)
              :sql-name (col-name slot)
              :type (col-type slot)
              :null-p (col-null-p slot)
              :unique-p (col-null-p slot)
              :index-p (col-index-p slot)
              :default (col-default slot))))

(defmethod digest-slots ((class-name symbol))
  (digest-slots (find-class class-name)))

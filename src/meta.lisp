(in-package :crane)
(annot:enable-annot-syntax)

(defclass table-class (closer-mop:standard-class)
  ((table-name :reader table-class-name :initarg :table-name)
   (abstractp :reader table-class-abstract-p :initarg :abstractp :initform nil)))

(defmethod table-name ((class table-class))
  (if (slot-boundp class 'table-name)
      (car (table-class-name class))
      (class-name class)))

(defmethod table-name ((class-name symbol))
  (table-name (find-class class-name)))

(defmethod abstractp ((class table-class))
  (car (table-class-abstract-p class)))

(defmethod abstractp ((class-name symbol))
  (abstractp (find-class class-name)))

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
   (col-primary-p :initarg :col-primary-p
                  :reader col-primary-p
                  :initform nil)
   (col-index-p :initarg :col-index-p
                :reader  col-index-p
                :initform nil)
   (col-default :initarg :col-default
                :reader  col-default)
   (col-check :initarg :col-check
              :reader col-check
              :initarg nil)))

(defclass table-class-effective-slot-definition (closer-mop:standard-effective-slot-definition)
  ((col-type :initarg :col-type
             :accessor col-type)
   (col-null-p :initarg :col-null-p
               :reader  col-null-p)
   (col-unique-p :initarg :col-unique-p
                 :reader  col-unique-p)
   (col-primary-p :initarg :col-primary-p
                  :reader col-primary-p)
   (col-index-p :initarg :col-index-p
                :reader  col-index-p)
   (col-default :initarg :col-default
                :reader  col-default)
   (col-check :initarg :col-check
              :reader col-check)))

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

          (slot-value effective-slot-definition 'col-primary-p)
          (col-primary-p (first direct-slot-definitions))

          (slot-value effective-slot-definition 'col-index-p)
          (col-index-p (first direct-slot-definitions))
          
          (slot-value effective-slot-definition 'col-check)
          (if (slot-boundp (first direct-slot-definitions) 'col-check)
              (col-check (first direct-slot-definitions))
              nil))
    (if (slot-boundp (first direct-slot-definitions) 'col-default)
        (setf (slot-value effective-slot-definition 'col-default)
              (col-default (first direct-slot-definitions))))
    effective-slot-definition))

(defun digest-slot (slot)
  (append
   (list :name (closer-mop:slot-definition-name slot)
         :type (col-type slot)
         :null-p (col-null-p slot)
         :unique-p (col-null-p slot)
         :primar-p (col-primary-p slot)
         :index-p (col-index-p slot)
         :check (col-check slot))
   (if (slot-boundp slot 'col-default)
       (list :default (col-default slot))
       nil)))

(defmethod digest ((class table-class))
  "Serialize a class's options and slots' options into a plist"
  (list nil
        (let ((slots (closer-mop:class-slots class)))
          (if slots
              (mapcar #'digest-slot
                (closer-mop:class-slots class))
              (error 'crane.errors:empty-table
                      :text "The table ~A has no slots."
                      (table-name class))))))

@export
(defmethod digest ((class-name symbol))
  (digest (find-class class-name)))

(defun diff-slot (slot-a slot-b)
  "Compute the difference between two slot digests.
See DIGEST."
  (crane.utils:diff-plist slot-a slot-b :test #'equal))

(defun sort-slot-list (list)
  list)

@export
(defun diff-digest (digest-a digest-b)
  "Compute the difference between two digests.
See DIGEST."
  (remove-if #'null
             (mapcar #'diff-slot
                     (sort-slot-list (second digest-a))
                     (sort-slot-list (second digest-b)))))

(defmethod initialize-instance :after ((class table-class) &key)
  (format t "Initializing class~&")
  (closer-mop:finalize-inheritance class)
  (if (crane.migration:migration-history-p (table-name class))
      (progn
        (format t "Class already defined. Comparing digests...~&")
        (aif (diff-digest
              (digest class)
              (crane.migration:get-last-migration (table-name class)))
            (crane.migration:migrate class if)
            (format t "No differences.~&")))
      (progn
        (format t "Class defined for the first time. Creating file...~&")
        (crane.migration:insert-migration (table-name class)
                                          (digest class)))))

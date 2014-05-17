(defpackage :crane.table
  (:use :cl :anaphora :cl-annot.doc :iter)
  (:export :<table>))
(in-package :crane.table)
(annot:enable-annot-syntax)

(defparameter +slot-mapping+
  (list :type           :col-type
        :nullp          :col-null-p
        :uniquep        :col-unique-p
        :primaryp       :col-primary-p
        :indexp         :col-index-p
        :check          :col-check
        :autoincrementp :col-autoincrement-p
        :foreign        :col-foreign))

(defparameter +standard-class-options+
  (list :initarg :initform :accessor :reader :writer))

@doc "If the slot doesn't have :initarg or :accessor slots, add them"
(defun add-default-slots (slot-name plist)
  (let ((plist-with-accessor
          (if (not (member :accessor plist))
              (append (list :accessor slot-name) plist)
              plist)))
    (if (not (member :initarg plist))
        (append (list :initarg (intern (symbol-name slot-name) :keyword))
                plist-with-accessor)
        plist-with-accessor)))
      

@doc "Take a plist like (:col-type 'string :col-null-p t) and remove the
prefixes on the keys. Turn 'deftable slot properties' (:type, :nullp, etc.) into
'table-class slot properties' (:col-type, :col-null-p, etc.)"
(defun process-slot (slot)
  (cons (car slot)
        (add-default-slots (car slot)
         (iter (for (key val) on (cdr slot) by #'cddr)
               (appending (if (member key +standard-class-options+)
                              (list key val)
                              (list (getf +slot-mapping+ key) val)))))))

@doc "To minimize the number of parentheses, both slots and table options come
in the same list. This function separates them: Normal slot names are plain old
symbols, table options are keywords."
(defun separate-slots-and-options (slots-and-options)
  (let ((slots (list))
        (options (list)))
    (iter (for item in slots-and-options)
      (if (eq (symbol-package (car item)) (find-package :keyword))
          (push item options)
          (push (process-slot item) slots)))
    (list slots options)))

(defclass <table> () ()
  (:metaclass crane.meta:table-class))

(defun any-concrete-superclasses (superclasses)
  (remove-if #'crane.meta:abstractp superclasses))

@export
(defmacro deftable (name (&rest superclasses) &rest slots-and-options)
  (destructuring-bind (slots options)
      (separate-slots-and-options slots-and-options)
    `(progn
       (defclass ,name ,(if superclasses superclasses `(crane.table:<table>))
         ,(append
           (when (or
                  ;; If it's abstract, it doesn't have an ID
                  (not (cadr (assoc :abstractp options)))
                  ;; If it's concrete, it has an ID, unless it has a concrete
                  ;; superclass
                  (any-concrete-superclasses superclasses))
               `((,(intern "ID" *package*)
                  :col-type integer
                  :col-primary-p t
                  :col-null-p nil
                  :initform (1+ (crane.query:latest-id (find-class ',name)))
                  :accessor ,(intern "ID" *package*)
                  :autoincrement t
                  :initarg :id)))
           slots)
         ,@options
         (:metaclass crane.meta:table-class))
       (closer-mop:finalize-inheritance (find-class ',name))
       (unless (crane.meta:deferredp ',name)
         (crane.migration:build ',name)))))

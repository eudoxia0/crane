(in-package :crane)
(annot:enable-annot-syntax)

(defparameter +slot-mapping+
  (list :type     :col-type
        :nullp    :col-null-p
        :uniquep  :col-unique-p
        :primaryp :col-primary-p
        :indexp   :col-index-p
        :check    :col-check))

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
  (:metaclass table-class))

@export
(defmacro deftable (name (&rest superclasses) &rest slots-and-options)
  (destructuring-bind (slots options)
      (separate-slots-and-options slots-and-options)
    `(progn
       (defclass ,name ,(if superclasses superclasses `(crane:<table>))
         ((,(intern "ID" *package*) :col-type integer :col-primary-p t :col-null-p nil
              :initform (1+ (crane::latest-id (find-class ',name)))
              :initarg :id)
          ,@slots)
         ,@options
         (:metaclass crane:table-class))
       (closer-mop:finalize-inheritance (find-class ',name))
       (crane:build ',name))))

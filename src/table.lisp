(in-package :crane)

(defun process-slot (slot)
  slot)

(defun separate-slots-and-options (slots-and-options)
  (let ((slots (list))
        (options (list)))
    (iter (for item in slots-and-options)
      (if (eq (symbol-package (car item)) (find-package :keyword))
          (push item options)
          (push (process-slot item) slots)))
    (list slots options)))

(defmacro deftable (name (&rest superclasses) &rest slots-and-options)
  (destructuring-bind (slots options) (separate-slots-and-options slots-and-options)
    `(progn
       (defclass ,name ,superclasses
         ,slots
         ,@options
         (:metaclass crane:table-class))
       (closer-mop:finalize-inheritance (find-class (quote ,name)))
       (crane.meta:build name))))

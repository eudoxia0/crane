(in-package :crane)
(annot:enable-annot-syntax)

@doc "Take a plist like (:col-type 'string :col-null-p t) and remove the
prefixes on the keys."
(defun process-slot (slot)
  slot)

@doc "To minimize the number of parentheses, both slots and table options come
in the same list. This function separates them: Normal slot names are plain old
symbols, table options are keywords."
(defun separate-slots-and-options (slots-and-options)
  (let ((slots (list))
        (options (list)))
    (iter (for item in slots-and-options)
      (if (eq (symbol-package (car item)) (find-package :keyword))
          (push item options)
          (push (process-slot (cdr item)) slots)))
    (list slots options)))

@export
(defmacro deftable (name (&rest superclasses) &rest slots-and-options)
  (destructuring-bind (slots options) (separate-slots-and-options slots-and-options)
    `(progn
       (defclass ,name ,superclasses
         ,slots
         ,@options
         (:metaclass crane:table-class))
       (closer-mop:finalize-inheritance (find-class ',name))
       (crane:build ',name))))

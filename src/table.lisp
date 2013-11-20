(in-package :crane)

(defun separate-slots-and-options (slots-and-options)
  (let ((slots (list))
        (options (list)))
    (loop for item in slots-and-options do
      (if (eq (symbol-package (car item)) (find-package :keyword))
          (push item options)
          (push item slots)))
    (list slots options)))

(defmacro deftable (name (&rest superclasses) &rest slots-and-options)
  (destructuring-bind (slots options) (separate-slots-and-options slots-and-options)
    `(defclass ,name ,superclasses
       ,slots
       ,@options
       (:metaclass crane:table-class))))

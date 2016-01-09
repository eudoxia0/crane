(defpackage crane-test.table
  (:use :cl :fiveam)
  (:export :table-tests))
(in-package :crane-test.table)

(def-suite table-tests
  :description "Table definition tests.")
(in-suite table-tests)

(test define-person
  (finishes
    (crane.table:deftable person ()
      ((age :accessor person-age
            :initarg :age
            :initform 0
            :type crane.types:int
            :documentation "The person's age.")))))

(test person-table
  (let ((class (find-class 'person)))
    (is-false
     (crane.table:table-abstract-p class))
    (is
     (equal (crane.table:table-name class)
            "\"person\""))
    (is
     (equal (length (crane.table:table-columns class))
            2)))
  (let ((person (make-instance 'person :age 10)))
    (is
     (typep person 'person))
    (is
     (equal (person-age person)
            10))))

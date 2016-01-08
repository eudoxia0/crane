(defpackage crane-test.serialize
  (:use :cl :fiveam)
  (:export :serialization-tests))
(in-package :crane-test.serialize)

(def-suite serialization-tests
  :description "Serialization tests.")
(in-suite serialization-tests)

(test serialize
  (is
   (listp
    (crane.serialize:serialize (find-class 'crane-test.table::person)))))

(test deserialize
  (is
   (typep (crane.serialize:deserialize
           (crane.serialize:serialize
            (find-class 'crane-test.table::person)))
          'crane.table:table-class)))

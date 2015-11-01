(defpackage crane-test.serialize
  (:use :cl :fiveam)
  (:export :serialization-tests))
(in-package :crane-test.serialize)

(def-suite serialization-tests
  :description "Serialization tests.")
(in-suite serialization-tests)

(test column
  (is
   (stringp
    (yason:with-output-to-string* (:indent t)
      (yason:encode-object
       (first
        (crane.table:table-columns (find-class 'crane-test.table::person))))))))

(test table
  (is
   (stringp
    (yason:with-output-to-string* (:indent t)
      (yason:encode-object
       (find-class 'crane-test.table::person))))))

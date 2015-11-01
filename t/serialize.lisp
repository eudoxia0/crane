(defpackage crane-test.serialize
  (:use :cl :fiveam)
  (:export :serialization-tests))
(in-package :crane-test.serialize)

(def-suite serialization-tests
  :description "Serialization tests.")
(in-suite serialization-tests)

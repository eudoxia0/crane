(defpackage crane-test.diff
  (:use :cl :fiveam)
  (:export :diff-tests))
(in-package :crane-test.diff)

(def-suite diff-tests
  :description "Table diff tests.")
(in-suite diff-tests)

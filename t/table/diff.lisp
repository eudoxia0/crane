(defpackage crane-test.table.diff
  (:use :cl :fiveam)
  (:export :diff-tests))
(in-package :crane-test.table.diff)

(def-suite diff-tests
  :description "Table diff tests.")
(in-suite diff-tests)

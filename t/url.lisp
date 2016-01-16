(defpackage crane-test.url
  (:use :cl :fiveam)
  (:export :url-tests))
(in-package :crane-test.url)

(def-suite url-tests
  :description "Database URL tests.")
(in-suite url-tests)

(in-package :cl-user)
(defpackage crane-test
  (:use :cl :crane :fiveam)
  (:export :setup))

(defpackage crane-test.util
  (:use :cl :fiveam)
  (:export :util))

(defpackage crane-test.spec
  (:use :cl :crane :fiveam)
  (:export :postgres
           :mysql
           :sqlite3))

(defpackage crane-test.generic
  (:use :cl :crane :fiveam)
  (:export :table-slots
           :column-slots
           :foreign
           :dereferencing
           :migrations
           :queries))

(defpackage crane-test.postgres
  (:use :cl :crane :fiveam)
  (:export :config))

(defpackage crane-test.mysql
  (:use :cl :crane :fiveam))

(defpackage crane-test.sqlite3
  (:use :cl :crane :fiveam))

(defpackage crane-test.final
  (:use :cl :fiveam))

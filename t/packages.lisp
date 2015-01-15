(in-package :cl-user)
(defpackage crane-test.util
  (:use :cl :fiveam)
  (:export :util-tests))

(defpackage crane-test.spec
  (:use :cl :fiveam)
  (:export :postgres
           :mysql
           :sqlite3))

(defpackage crane-test.postgres
  (:use :cl :crane :fiveam)
  (:export :config
           :table-slots
           :column-slots
           :foreign
           :migrations
           :queries
           :inflate-deflate))

(defpackage crane-test.mysql
  (:use :cl :crane :fiveam))

(defpackage crane-test.sqlite3
  (:use :cl :crane :fiveam)
  (:export :config
           :tables
           :queries))

(defpackage crane-test
  (:use :cl :fiveam))

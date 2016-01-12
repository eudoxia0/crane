(defpackage crane-test.foreign
  (:use :cl :fiveam)
  (:import-from :crane.table
                :deftable)
  (:export :foreign-tests))
(in-package :crane-test.foreign)

(def-suite foreign-tests
  :description "Foreign key tests.")
(in-suite foreign-tests)

(deftable person ()
  ((name :reader person-name
         :initarg :name
         :type crane.types:text))
  (:abstractp t))

(deftable parent (person)
  ())

(deftable child (person)
  ((parent :reader child-parent
           :initarg :parent
           :type crane.types:int
           :foreign (parent))))

(defun test-database (database-tag)
  (let ((session (crane.session:make-session :migratep nil)))
    (crane.session:register-database session database-tag)
    (crane.session:register-table session 'parent database-tag)
    (crane.session:register-table session 'child database-tag)
    (crane.session:start session)
    (crane.session:stop session)))

(test postgres-foreign
  (test-database 'crane-test.config:pg))

(test mysql-foreign
  (test-database 'crane-test.config:mysql))

(test sqlite3-foreign
  (test-database 'crane-test.config:sqlite))

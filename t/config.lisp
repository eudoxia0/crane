(defpackage crane-test.config
  (:use :cl :fiveam)
  (:import-from :crane-test
                :*postgres-database*
                :*postgres-username*
                :*postgres-password*
                :*mysql-database*
                :*mysql-username*
                :*mysql-password*
                :*sqlite3-pathname*)
  (:export :config-tests))
(in-package :crane-test.config)

(def-suite config-tests
  :description "Configuration tests.")
(in-suite config-tests)

(test definition
  (finishes
    (crane.config:define-postgres-database pg
      :name *postgres-database*
      :username *postgres-username*
      :password *postgres-password*))
  (finishes
    (crane.config:define-mysql-database mysql
      :name *mysql-database*
      :username *mysql-username*
      :password *mysql-password*))
  (finishes
    (crane.config:define-sqlite3-database sqlite
      :pathname *sqlite3-pathname*)))

(test queries
  (let ((number 0))
    (finishes
      (crane.config:do-databases (tag database)
        (incf number)))
    (is
     (equal number 3)))
  (finishes
   (crane.config:list-databases)))

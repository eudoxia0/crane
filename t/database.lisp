(defpackage crane-test.database
  (:use :cl :fiveam)
  (:import-from :crane-test
                :*postgres-database*
                :*postgres-username*
                :*postgres-password*
                :*mysql-database*
                :*mysql-username*
                :*mysql-password*
                :*sqlite3-pathname*)
  (:export :database-tests))
(in-package :crane-test.database)

(def-suite database-tests
  :description "Database tests.")
(in-suite database-tests)

(defun tests (database)
  (is-true
   (crane.database:connect database))
  (let ((result (crane.database:sql-query database "SELECT 1+1" nil)))
    (is
     (equal (cadr (dbi:fetch result))
            2)))
  (unless (typep database 'crane.database.postgres:postgres)
    (let ((result (crane.database:query database
                                        (sxql:select ((:+ 1 1))))))
      (is
       (equal (cadr (dbi:fetch result))
              2))))
  (is-true
   (crane.database:disconnect database)))

(test postgres
  (let ((database (make-instance 'crane.database.postgres:postgres
                                 :name *postgres-database*
                                 :username *postgres-username*
                                 :password *postgres-password*)))
    (tests database)))

(test mysql
  (let ((database (make-instance 'crane.database.mysql:mysql
                                 :name *mysql-database*
                                 :username *mysql-username*
                                 :password *mysql-password*)))
    (tests database)))

(test sqlite3
  (let ((database (make-instance 'crane.database.sqlite3:sqlite3
                                 :name (namestring *sqlite3-pathname*))))
    (tests database)))

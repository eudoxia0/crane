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
  ;; disconnecting before connection errors
  (signals error
    (crane.database:disconnect database))
  ;; connect
  (is-true
   (crane.database:connect database))
  ;; simple queries
  (is
   (equal (second
           (dbi:fetch
            (crane.database:sql-query database "SELECT 1+1" nil)))
          2))
  (is
   (equal (second
           (dbi:fetch
            (crane.database:sql-query database "SELECT 1+?" (list 1))))
          2))
  ;; table existence
  (is-false
   (crane.database:table-exists-p database "my_table"))
  ;; disconnect
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

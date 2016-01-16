(defpackage crane-test.url
  (:use :cl :fiveam)
  (:export :url-tests))
(in-package :crane-test.url)

(def-suite url-tests
  :description "Database URL tests.")
(in-suite url-tests)

(test postgres
  (let ((db (crane.url:parse "postgres://user:pass@host:1234/db")))
    (is
     (string= (crane.database.postgres:database-name db)
              "db"))
    (is
     (string= (crane.database.postgres:database-username db)
              "user"))
    (is
     (string= (crane.database.postgres:database-password db)
              "pass"))
    (is
     (string= (crane.database.postgres:database-host db)
              "host"))
    (is
     (= (crane.database.postgres:database-port db)
        1234))))

(test mysql
  (let ((db (crane.url:parse "mysql://user:pass@host:1234/db")))
    (is
     (string= (crane.database.mysql:database-name db)
              "db"))
    (is
     (string= (crane.database.mysql:database-username db)
              "user"))
    (is
     (string= (crane.database.mysql:database-password db)
              "pass"))
    (is
     (string= (crane.database.mysql:database-host db)
              "host"))
    (is
     (= (crane.database.mysql:database-port db)
        1234))))

(in-package :cl-user)
(defpackage crane-test
  (:use :cl :fiveam)
  (:export :*postgres-database*
           :*postgres-username*
           :*postgres-password*
           :*mysql-database*
           :*mysql-username*
           :*mysql-password*
           :*sqlite3-pathname*
   :run-tests))
(in-package :crane-test)

;;; Postgres

(defvar *postgres-database* "postgres_db")

(defvar *postgres-username* "postgres_user")

(defvar *postgres-password* "postgres_pass")

;;; MySQL

(defvar *mysql-database* "mysql_db")

(defvar *mysql-username* "mysql_user")

(defvar *mysql-password* "mysql_pass")

;;; SQLite3

(defvar *sqlite3-pathname*
  (asdf:system-relative-pathname :crane #p"t/sqlite3.db"))

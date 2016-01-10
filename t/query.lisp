(defpackage crane-test.query
  (:use :cl :fiveam)
  (:export :query-tests))
(in-package :crane-test.query)

(def-suite query-tests
  :description "Query tests.")
(in-suite query-tests)

(crane.table:deftable all ()
  ((b :reader all-b
      :initarg :b
      :type crane.types:bool)
   (i :reader all-i
      :initarg :i
      :type crane.types:int)
   (bi :reader all-bi
       :initarg :bi
       :type crane.types:bigint)
   (si :reader all-si
       :initarg :si
       :type crane.types:smallint)
   (f :reader all-f
      :initarg :f
      :type crane.types:double)
   (txt :reader all-txt
        :initarg :txt
        :type crane.types:text)))

(defun test-database (database-tag)
  (let ((session (make-session :migratep nil)))
    (register-database session database-tag)
    (register-table session 'all database-tag)
    (crane.session:start session)
    ;; With default
    (let ((crane.session:*session* session))
      ;; Create an instance
      (let ((instance (crane.query:create 'all
                                          :b t
                                          :i 10
                                          :bi 10000000000
                                          :si 10
                                          :f 3.14
                                          :txt "text")))
        t))
    ;; Final
    (crane.session:stop session)))

(test postgres-queries
  (test-database 'crane-test.config:pg))

(test mysql-queries
  (test-database 'crane-test.config:mysql))

(test sqlite3-queries
  (test-database 'crane-test.config:sqlite))

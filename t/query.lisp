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
        :type crane.types:text)
   (stamp :reader all-stamp
          :initarg :stamp
          :type crane.types:timestamp)))

(defun test-database (database-tag)
  (let ((session (crane.session:make-session :migratep nil)))
    (crane.session:register-database session database-tag)
    (crane.session:register-table session 'all database-tag)
    (crane.session:start session)
    ;; With default
    (let ((crane.session:*session* session)
          (now (local-time:now)))
      ;; Count the number of instances
      (is
       (equal (crane.query:total 'all) 0))
      ;; Create an instance
      (let ((instance (crane.query:create 'all
                                          :b t
                                          :i 10
                                          :bi 10000000000
                                          :si 10
                                          :f 3.14
                                          :txt "text"
                                          :stamp now)))
        (is
         (equal (crane.query:total 'all) 1))
        (let ((restored (crane.query:single 'all)))
          (is
           (typep restored 'all))
          (is
           (eql (all-b restored) t))
          (is
           (= (all-i restored) 10))
          (is
           (= (all-bi restored) 10000000000))
          (is
           (= (all-si restored) 10))
          (is
           (= (coerce (all-f restored) 'single-float) 3.14))
          (is
           (string= (all-txt restored) "text"))
          (is
           (local-time:timestamp= (all-stamp restored) now)))))
    ;; Final
    (crane.session:stop session)))

(test postgres-queries
  (test-database 'crane-test.config:pg))

(test mysql-queries
  (test-database 'crane-test.config:mysql))

(test sqlite3-queries
  (test-database 'crane-test.config:sqlite))

(defpackage crane-test.session
  (:use :cl :fiveam)
  (:import-from :crane.session
                :make-session
                :session
                :session-databases
                :register-database
                :register-table)
  (:export :session-tests))
(in-package :crane-test.session)

(def-suite session-tests
  :description "Session tests.")
(in-suite session-tests)

(crane.table:deftable truck ()
  ((model :reader truck-model
          :initarg :model
          :type crane.types:text)
   (mileage :reader truck-mileage
            :initarg :mileage
            :type crane.types:int)))

(defun test-session (database-tag)
  (let ((session (make-session :migratep nil)))
    (is
     (typep session 'session))
    (is
     (null (session-databases session)))
    (finishes
      (register-database session database-tag))
    (is
     (equal (length (session-databases session))
            1))
    (finishes
      (register-table session 'truck database-tag))
    (finishes
      (crane.session:start session))
    (is-true
     (crane.database:table-exists-p (crane.config:get-database database-tag)
                                    (crane.table:table-name
                                     (find-class 'truck))))
    (finishes
      ;; Starting a session again does nothing
      (crane.session:start session))
    ;; Queries
    (let ((instance (make-instance 'truck
                                   :model "abc"
                                   :mileage 50)))
      (is
       (integerp (crane.session:create session instance))))
    (finishes
      (crane.session:stop session))))

(test postgres-session
  (test-session 'crane-test.config:pg))

(test mysql-session
  (test-session 'crane-test.config:mysql))

(test sqlite3-session
  (test-session 'crane-test.config:sqlite))

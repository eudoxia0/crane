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

(test basic
  (finishes
    (crane.config:define-sqlite3-database memory
      :name ":memory:"))
  (let ((session (make-session :migratep nil)))
    (is
     (typep session 'session))
    (is
     (null (session-databases session)))
    (finishes
      (register-database session 'memory))
    (is
     (equal (length (session-databases session))
            1))
    (finishes
      (register-table session 'truck 'memory))
    (finishes
      (crane.session:start session))
    (is-true
     (crane.database:table-exists-p (crane.config:get-database 'memory)
                                    (crane.table:table-name
                                     (find-class 'truck))))
    (finishes
      ;; Starting a session again does nothing
      (crane.session:start session))
    (finishes
      (crane.session:stop session))))

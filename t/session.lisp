(defpackage crane-test.session
  (:use :cl :fiveam)
  (:import-from :crane-test.config
                :sqlite)
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
  (let ((session (make-session :migratep nil)))
    (is
     (typep session 'session))
    (is
     (null (session-databases session)))
    (finishes
      (register-database session 'sqlite))
    (is
     (equal (length (session-databases session))
            1))
    (finishes
      (register-table session 'truck 'sqlite))
    (finishes
      (crane.session:start session))
    (finishes
      (crane.session:start session))))

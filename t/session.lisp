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
  ((model :accessor truck-model
          :initarg :model
          :type crane.types:text)
   (mileage :accessor truck-mileage
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
      (is-false
       (crane.session:exists-in-database-p session instance))
      (is
       (typep (crane.session:create session instance) 'truck))
      (is
       (integerp (crane.table:id instance)))
      (is-true
       (crane.session:exists-in-database-p session instance))
      ;; Make changes, save them, verify they happen
      (let ((results (dbi:fetch
                      (crane.session:select '(:model :mileage)
                                            session
                                            'truck
                                            (list := :id (crane.table:id instance))))))
        (is
         (string= (getf results :|model|) "abc"))
        (is
         (= (getf results :|mileage|) 50))
        (setf (truck-model instance) "xyz"
              (truck-mileage instance) 70)
        (finishes
          (crane.session:save session instance))
        (let ((results (dbi:fetch
                        (crane.session:select '(:model :mileage)
                                              session
                                              'truck
                                              (list := :id (crane.table:id instance))))))
          (is
           (string= (getf results :|model|) "xyz"))
          (is
           (= (getf results :|mileage|) 70))))
      ;; Filtering
      (let ((instances (crane.session:filter session
                                             'truck
                                             '(:> :mileage 60))))
        (is
         (= (length instances) 1)))
      ;; Delete
      (finishes
        (crane.session:delete-instance session instance)))
    (finishes
      (crane.session:stop session))))

(test postgres-session
  (test-session 'crane-test.config:pg))

(test mysql-session
  (test-session 'crane-test.config:mysql))

(test sqlite3-session
  (test-session 'crane-test.config:sqlite))

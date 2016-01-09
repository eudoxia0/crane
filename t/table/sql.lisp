(defpackage crane-test.table.sql
  (:use :cl :fiveam)
  (:export :sql-dsl))
(in-package :crane-test.table.sql)

(def-suite sql-dsl
  :description "Tests of the SQL DSL.")
(in-suite sql-dsl)

(test sqlize
  (is
   (string= (crane.table.sql::symbol-to-sql 'test)
            "\"test\"")))

(test constraint-sql
  (mapcar #'(lambda (constraint)
              (is
               (stringp (crane.table.sql::constraint-sql constraint))))
          (list (make-instance 'crane.table.sql::unique
                               :columns (list "a"))
                (make-instance 'crane.table.sql::not-null
                               :column "a")
                (make-instance 'crane.table.sql::primary-key
                               :columns (list "a")))))

(crane.table:deftable astronaut ()
  ((name :reader name
         :initarg :name
         :type crane.types:text
         :documentation "The name."))
  (:documentation "An astronaut."))

(test table-definition
  (let* ((db (make-instance 'crane.database.sqlite3:sqlite3
                            :name ":memory:"))
         (def (crane.table.sql::make-table-definition (find-class 'astronaut) db)))
    (is
     (typep def 'crane.table.sql::table-definition))
    (is
     (equal (crane.table.sql::table-definition-name def)
            "\"astronaut\""))
    (is
     (equal (length (crane.table.sql::table-definition-columns def))
            2))
    (let ((sql (crane.table.sql::table-definition-sql def)))
      (is (listp sql))
      (is
       (every #'stringp sql)))))

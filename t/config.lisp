(in-package :cl-user)
(defpackage crane-test
  (:use :cl :crane :fiveam))
(in-package :crane-test)

(crane:setup
 :debug t
 :migrations-directory
 (merge-pathnames
  #p"t/migrations/"
  (asdf:system-source-directory :crane-test))
 :databases
 (list :main
       (cond
         ((member :crane-use-postgres *features*)
          '(:type :postgres
            :name "crane_test_db"
            :user "crane_test_user"
            :pass "crane_test_user"))
         ((member :crane-use-sqlite3 *features*)
          '(:type :sqlite3
            :name ":memory:")))))

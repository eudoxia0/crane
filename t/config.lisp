(in-package :crane-test)

(crane:setup
 `(:migrations-directory
   ,(merge-pathnames
     #p"t/migrations/"
     (asdf:component-pathname (asdf:find-system :crane-test)))
   :databases
   (:main
    (:type :postgres
     :name "crane_test_db"
     :user "crane_test_user"
     :pass "crane_test_user")
    :interface
    (:type :sqlite3
     :name ":memory:"))))

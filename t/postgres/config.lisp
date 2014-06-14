(in-package :crane-test.postgres)

(def-suite config
  :description "Test that we can configure Crane and connect to a DB.")
(in-suite config)

(crane:setup
 :debug t
 :migrations-directory
 (merge-pathnames
  #p"t/migrations/"
  (asdf:system-source-directory :crane-test))
 :databases
 '(:main
   (:type :postgres
    :name "crane_test_db"
    :user "crane_test_user"
    :pass "crane_test_user")))

(test connect
  (finishes
    (crane.connect:connect)))

(test main-db
  (is (equal :main crane.connect:*default-db*)))

(run! 'config)

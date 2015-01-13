(in-package :crane-test.postgres)

(def-suite config
  :description "Test that we can configure Crane and connect to a DB.")
(in-suite config)

(test setup
  (finishes
   (crane:setup
    :debug t
    :migrations-directory
    (asdf:system-relative-pathname :crane-test #p"t/migrations/")
    :databases
    '(:postgres-db
      (:type :postgres
       :name "crane_test_db"
       :user "crane_test_user"
       :pass "crane_test_user")))))

(test (main-db :depends-on setup)
  (is (equal :postgres-db crane.connect:*default-db*)))

(test (connect :depends-on main-db)
  (finishes
    (crane.connect:connect)))

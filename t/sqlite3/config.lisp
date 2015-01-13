(in-package :crane-test.sqlite3)

(def-suite config
  :description "Test that we can configure Crane and connect to a DB.")
(in-suite config)

(test setup
  (finishes
   (crane:setup
    :debug t
    :migrations-directory
    (merge-pathnames
     #p"t/migrations/"
     (asdf:system-source-directory :crane-test))
    :databases
    '(:sqlite-db
      (:type :sqlite3
       :name ":memory:"))))

(test (main-db :depends-on setup)
  (is (equal :sqlite-db crane.connect:*default-db*)))

(test (connect :depends-on main-db)
  (finishes
    (crane.connect:connect)))

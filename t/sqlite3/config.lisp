(in-package :crane-test.sqlite3)

(def-suite sqlite3
  :description "SQLite3 tests.")
(in-suite sqlite3)

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
       :name ":memory:")))))

(test (main-db :depends-on setup)
  (is (equal :sqlite-db crane.connect:*default-db*)))

(test (connect :depends-on main-db)
  (finishes
    (crane.connect:connect)))

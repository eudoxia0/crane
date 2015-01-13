(in-package :crane-test.sqlite3)

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
 '(:sqlite-db
   (:type :sqlite3
    :name ":memory:")))

(test main-db
  (is (equal :sqlite-db crane.connect:*default-db*)))

(test connect
  (finishes
    (crane.connect:connect)))

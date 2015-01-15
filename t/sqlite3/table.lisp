(in-package :crane-test.sqlite3)

(def-suite tables
  :description "Test that we can configure Crane and connect to a DB.")
(in-suite tables)

(test create-basic-tables
  (finishes
    (deftable sq-table-a ()
      (field-a :type integer :nullp t)))
  (finishes
    (deftable sq-table-b (sq-table-a)
      (field-b :type text))))

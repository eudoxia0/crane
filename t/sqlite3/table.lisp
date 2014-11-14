(in-package :crane-test.sqlite3)

(test create-basic-tables
  (finishes
    (deftable sq-table-a ()
      (field-a :type integer :nullp t)))
  (finishes
    (deftable sq-table-b (sq-table-a)
      (field-b :type text))))

;;; Here, we test that we can define tables *before* connecting

(test connect
  (finishes
    (crane.connect:connect)))

(run! 'create-basic-tables)

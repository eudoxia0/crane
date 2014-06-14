(in-package :crane-test.sqlite3)

(test create-basic-tables
  (finishes
    (deftable table-a ()
      (field-a :type integer :nullp t)))
  (finishes
    (deftable table-b ()
      (field-b :type text :uniquep t))))

(run! 'create-basic-tables)

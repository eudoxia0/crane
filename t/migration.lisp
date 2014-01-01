(in-package :crane-test)

(def-suite migrations
  :description "Test that migrations actually work.")
(in-suite migrations)

(test simple-migration
  (finishes
    (deftable table-d (table-a)
      (field-b :type integer :unique t))))

(run! 'migrations)

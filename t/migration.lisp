(in-package :crane-test)

(def-suite migrations
  :description "Test that migrations actually work.")
(in-suite migrations)

(test simple-migration
  (finishes
    (deftable table-a ()
      (field-a :type string :nullp nil))))

(run! 'migrations)

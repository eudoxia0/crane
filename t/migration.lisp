(in-package :crane-test)

(def-suite migrations
  :description "Test that migrations actually work.")
(in-suite migrations)

(test initial-definitions
  (finishes
    (deftable a ()
      (field-a :type integer))

    (deftable b ()
      (field-a :type text :nullp nil))

    (deftable c ()
      (field-a :type integer :uniquep t))))

(test basic-changes
  (finishes
    (deftable a ()
      (field-a :type integer :uniquep t))

    (deftable b ()
      (field-a :type text :nullp t))

    (deftable c ()
      (field-a :type text :uniquep nil :primaryp p))))

(run! 'migrations)

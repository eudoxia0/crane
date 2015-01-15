(in-package :crane-test.postgres)

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
      (field-a :type integer :uniquep nil))))

(test basic-additions
  (finishes
    (deftable a ()
      (field-a :type integer :uniquep t)
      (field-b :type integer :nullp nil))

    (deftable b ()
      (field-a :type text :nullp t)
      (field-b :type integer :indexp t))))

(test basic-deletions
  (finishes
    (deftable a ()
      (field-a :type integer :uniquep t))))

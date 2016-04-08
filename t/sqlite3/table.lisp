(in-package :crane-test.sqlite3)

(test create-basic-tables
  (finishes
    (deftable sq-table-a ()
      (field-a :type integer :nullp t)))
  (finishes
    (deftable sq-table-b (sq-table-a)
      (field-b :type text))))

(test creating-related-tables
  (finishes
    (deftable sq-parent-table ()
      (something :type integer))

    (deftable sq-child-table ()
      (something-else :type text :initform "Foo")
      (ref :type integer :foreign (sq-parent-table :cascade :cascade)))))

(test (dereferencing :depends-on creating-related-tables)
  (is (equal
       (let* ((parent-instance (create 'sq-parent-table :something 99))
              (child-instance  (create 'sq-child-table :ref (id parent-instance))))
         (something (deref child-instance 'ref)))
       99)))

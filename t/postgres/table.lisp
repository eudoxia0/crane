(in-package :crane-test.postgres)

(test create-simple-table
  (finishes
    (deftable table-a ()
      (field-a :type text :nullp t))))

(test create-inherited-table
  (finishes
    (deftable table-b (table-a)
      (:abstractp t))))

(test create-another-table
  (finishes
    (deftable table-c (table-a)
      (field-b :type integer :nullp nil))))

(test find-tables
  (finishes
    (find-class 'table-a)
    (find-class 'table-b)
    (find-class 'table-c)))

(test column-options
  (finishes
    (deftable table-d (table-a)
      (field-b :type integer :default 1
               :unique-p t)))
  (finishes
    (closer-mop:class-slots (find-class 'table-d)))
  (finishes (crane.meta:digest (find-class 'table-d))))

(test creating-related-tables
  (finishes
    (deftable parent-table ()
      (something :type integer))

    (deftable child-table ()
      (something-else :type text :initform "Foo")
      (ref :type integer :foreign (parent-table :cascade :cascade)))))

(test (dereferencing :depends-on creating-related-tables)
  (is (equal
       (let* ((parent-instance (create 'parent-table :something 99))
              (child-instance  (create 'child-table :ref (id parent-instance))))
         (something (deref child-instance 'ref)))
       99)))

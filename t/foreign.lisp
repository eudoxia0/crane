(in-package :crane-test)

(def-suite foreign
  :description "Testing the 'R' in ORM.")
(in-suite foreign)

(test creating-related-tables
  (finishes
    (deftable parent-table ()
      (something :type integer))
   
    (deftable child-table ()
      (something-else :type text :initform "Foo")
      (ref :type integer :foreign (parent-table :on-delete :cascade)))))

(test (dereferencing :depends-on creating-related-tables)
  (is (equal
       (let* ((parent-instance (create 'parent-table :something 99))
              (child-instance  (create 'child-table :ref (id parent-instance))))
         (something (deref child-instance 'ref)))
       99)))

(run! 'foreign)

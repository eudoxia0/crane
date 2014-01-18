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
      (ref :type integer :foreign (parent-table)))))

(run! 'foreign)

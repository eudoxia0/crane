(in-package :crane-test)

(def-suite foreign
  :description "Testing the 'R' in ORM.")
(in-suite foreign)

(test creating-related-tables
  (finishes
    (deftable parent ()
      (something :type integer))
   
    (deftable child ()
      (something-else :type text :initform "Foo")
      (ref :type integer :foreign (parent)))))

(run! 'foreign)

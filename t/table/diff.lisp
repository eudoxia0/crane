(defpackage crane-test.table.diff
  (:use :cl :fiveam)
  (:import-from :crane.table.diff
                :difference
                :new-columns
                :old-columns
                :differences)
  (:export :diff-tests))
(in-package :crane-test.table.diff)

(def-suite diff-tests
  :description "Table diff tests.")
(in-suite diff-tests)

(crane.table:deftable alpha ()
  ((a :type crane.types:int)
   (b :type crane.types:int)
   (c :type crane.types:int)))

(crane.table:deftable beta ()
  ((a :type crane.types:int)
   (b :type crane.types:int)
   (c :type crane.types:int)
   (d :type crane.types:int)))

(crane.table:deftable gamma ()
  ((a :type crane.types:int)
   (b :type crane.types:int)))

(test same-table
  (let ((diff (differences alpha alpha)))
    (is
     (typep diff 'difference))
    (is
     (null (new-columns diff)))
    (is
     (null (old-columns diff)))))

(test addition
  (let ((diff (differences alpha beta)))
    (is
     (typep diff 'difference))
    (is
     (= (length (new-columns dif))
        1))
    (is
     (null (old-columns diff)))))

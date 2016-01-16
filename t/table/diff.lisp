(defpackage crane-test.table.diff
  (:use :cl :fiveam)
  (:import-from :crane.table.diff
                :difference
                :new-columns
                :old-columns
                :new-constraints
                :old-constraints
                :new-indices
                :old-indices
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

(crane.table:deftable delta ()
  ((b :type crane.types:int)
   (d :type crane.types:int)))

(defun class-to-storable (class)
  (crane.serialize:deserialize
   (crane.serialize:serialize class)))

(defun storable (class-name)
  (class-to-storable (find-class class-name)))

(defmacro test-diff ((diff a b) &body body)
  `(let ((,diff (differences (storable ',a) (storable ',b))))
     (is
      (typep ,diff 'difference))
     ,@body))

(test same-table
  (test-diff (diff alpha alpha)
    (is
     (null (new-columns diff)))
    (is
     (null (old-columns diff)))
    (is
     (null (new-constraints diff)))
    (is
     (null (old-constraints diff)))
    (is
     (null (new-indices diff)))
    (is
     (null (old-indices diff)))))

(test addition
  (test-diff (diff alpha beta)
    (is
     (= (length (new-columns diff))
        1))
    (is
     (null (old-columns diff)))))

(test deletion
  (test-diff (diff alpha gamma)
    (is
     (null (new-columns diff)))
    (is
     (= (length (old-columns diff))
        1))))

(test addition+deletion
  (test-diff (diff alpha delta)
    (is
     (= (length (new-columns diff))
        1))
    (is
     (= (length (old-columns diff))
        2))))

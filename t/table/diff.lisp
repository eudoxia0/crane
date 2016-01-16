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

;;; Column changes

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

(test column-addition
  (test-diff (diff alpha beta)
    (is
     (= (length (new-columns diff))
        1))
    (is
     (null (old-columns diff)))))

(test column-deletion
  (test-diff (diff alpha gamma)
    (is
     (null (new-columns diff)))
    (is
     (= (length (old-columns diff))
        1))))

(test column-addition+deletion
  (test-diff (diff alpha delta)
    (is
     (= (length (new-columns diff))
        1))
    (is
     (= (length (old-columns diff))
        2))))

;;; Column constrain changes

(crane.table:deftable c-alpha ()
  ((a :type crane.types:int
      :nullp nil)
   (b :type crane.types:int
      :nullp t)))

(crane.table:deftable c-beta ()
  ((a :type crane.types:int
      :nullp nil)
   (b :type crane.types:int
      :nullp nil)))

(test constraint-addition
  (test-diff (diff c-alpha c-beta)
    (is
     (= (length (new-constraints diff))
        1))
    (is
     (null (old-constraints diff)))))

(test constreaint-deletion
  (test-diff (diff c-beta c-alpha)
    (is
     (null (new-constraints diff)))
    (is
     (= (length (old-constraints diff))
        1))))

;;; Column index changes

(crane.table:deftable i-alpha ()
  ((a :type crane.types:int
      :indexp t)
   (b :type crane.types:int
      :indexp nil)))

(crane.table:deftable i-beta ()
  ((a :type crane.types:int
      :indexp t)
   (b :type crane.types:int
      :indexp t)))

(test index-addition
  (test-diff (diff i-alpha i-beta)
    (is
     (= (length (new-indices diff))
        1))
    (is
     (null (old-indices diff)))))

(test index-deletion
  (test-diff (diff i-beta i-alpha)
    (is
     (null (new-indices diff)))
    (is
     (= (length (old-indices diff))
        1))))

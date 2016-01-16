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

(defun class-to-storable (class)
  (crane.serialize:deserialize
   (crane.serialize:serialize class)))

(defmacro storable (class-name)
  `(class-to-storable (find-class ',class-name)))

(test same-table
  (let ((diff (differences (storable alpha) (storable alpha))))
    (is
     (typep diff 'difference))
    (is
     (null (new-columns diff)))
    (is
     (null (old-columns diff)))))

(test addition
  (let ((diff (differences (storable alpha) (storable beta))))
    (is
     (typep diff 'difference))
    (is
     (= (length (new-columns diff))
        1))
    (is
     (null (old-columns diff)))))

(test deletion
  (let ((diff (differences (storable alpha) (storable gamma))))
    (is
     (typep diff 'difference))
    (is
     (null (new-columns diff)))
    (is
     (= (length (old-columns diff))
        1))))

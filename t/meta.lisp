(in-package :crane-test)

(def-suite table-slots
    :description "Test that table metaclass slots work.")
(in-suite table-slots)

(defclass table-a ()
  ()
  (:metaclass crane:table-class))

(defclass table-b ()
  ()
  (:metaclass crane:table-class)
  (:abstract-p t))

(defclass table-c ()
  ()
  (:metaclass crane:table-class)
  (:abstract-p t)
  (:table-name table--c))

(test find-tables
  (finishes
    (find-class 'table-a)
    (find-class 'table-b)
    (find-class 'table-c)))

(test table-names
  (is (equal (table-name 'table-a) 'table-a)
      (equal (table-name 'table-b) 'table-b)
      (equal (table-name 'table-c) 'table--c)))

(run! 'table-slots)

(def-suite column-slots
    :description "Test that table column options work.")
(in-suite column-slots)

(defclass table-d ()
  ((field-a :col-type 'string :col-null-p t)
   (field-b :col-type 'integer :col-default 1
            :col-unique-p t))
  (:metaclass crane:table-class))

(test column-options
  (finishes
    (closer-mop:ensure-finalized (find-class 'table-d)))
  (finishes
    (closer-mop:class-slots (find-class 'table-d)))
  (finishes (crane::digest-slots (find-class 'table-d))))

(run! 'column-slots)

(in-package :crane-test)

;; Clean slate
;(fad:delete-directory-and-files
; (crane.migration::get-migration-dir))

(def-suite table-slots
    :description "Test that table metaclass slots work.")
(in-suite table-slots)

(defclass table-a ()
  ((field-a :col-type 'string :col-null-p t))
  (:metaclass crane:table-class))

(defclass table-b (table-a)
  ()
  (:metaclass crane:table-class)
  (:abstractp t))

(defclass table-c (table-b)
  ()
  (:metaclass crane:table-class)
  (:abstractp t)
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

(defclass table-d (table-a)
  ((field-b :col-type 'integer :col-default 1
            :col-unique-p t))
  (:metaclass crane:table-class))

(test column-options
  (finishes
    (closer-mop:class-slots (find-class 'table-d)))
  (finishes (crane::digest (find-class 'table-d))))

(run! 'column-slots)

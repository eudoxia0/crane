(in-package :crane-test)

;; Clean slate
(fad:delete-directory-and-files
 (crane.migration::get-migration-dir))

(def-suite table-slots
    :description "Test that table metaclass slots work.")
(in-suite table-slots)

(test create-tables
  (finishes
    (deftable table-a ()
      (field-a :type 'string :nullp t))
    
    (deftable table-b (table-a)
      (:abstractp t))
    
    (deftable table-c (table-a)
      (:abstractp t)
      (:table-name table--c))))
    
(test find-tables
  (finishes
    (find-class 'table-a)
    (find-class 'table-b)
    (find-class 'table-c)))

(test table-names
  (is (equal (table-name 'table-a) 'table-a)
      (equal (table-name 'table-b) 'table-b)
      (equal (table-name 'table-c) 'table--c)))

(def-suite column-slots
    :description "Test that table column options work.")
(in-suite column-slots)

(test column-options
  (finishes
    (deftable table-d (table-a)
      (field-b :type 'integer :default 1
               :unique-p t)))
  (finishes
    (closer-mop:class-slots (find-class 'table-d)))
  (finishes (crane::digest (find-class 'table-d))))

(run! 'table-slots)
(run! 'column-slots)

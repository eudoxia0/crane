(in-package :crane-test)

(def-suite preliminary
  :description "Start with a clean slate")
(in-suite preliminary)

(test drop-tables
  (finishes
    (format t "Dropping tables...~&")
    (handler-case
        (progn
          (delete-migrations t)
          
          (dolist (table '(table-a table-b table-c table-d
                           a b c child-table parent-table))
            (handler-case
                ;; Make sure no single failed delete takes down the whole thing
                (crane:drop-table table)
              (t () (format t "~&Failed to drop table '~A'.~&" table)))))
      (t () t))))

(def-suite table-slots
  :description "Test that table metaclass slots work.")
(in-suite table-slots)

(test create-simple-table
  (finishes
    (deftable table-a ()
      (field-a :type text :nullp t))))

(test create-inherited-table
  (finishes
    (deftable table-b (table-a)
      (:abstractp t))))

(test create-another-table
  (finishes
    (deftable table-c (table-a)
      (field-b :type integer :nullp nil)
      (:table-name table--c))))
    
(test find-tables
  (finishes
    (find-class 'table-a)
    (find-class 'table-b)
    (find-class 'table-c)))


(def-suite column-slots
  :description "Test that table column options work.")
(in-suite column-slots)

(test column-options
  (finishes
    (deftable table-d (table-a)
      (field-b :type integer :default 1
               :unique-p t)))
  (finishes
    (closer-mop:class-slots (find-class 'table-d)))
  (finishes (crane.meta:digest (find-class 'table-d))))

(run! 'preliminary)
(run! 'table-slots)
(run! 'column-slots)

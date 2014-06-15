(in-package :crane-test.sqlite3)

(def-suite queries
  :description "Creating, saving, and destroying objects.")
(in-suite queries)

(test creating
  (finishes
   (create 'sq-table-a :field-a 1)
   (create 'sq-table-a :field-a 2)
   (create 'sq-table-a :field-a 3)))

(test saving
  (finishes
    (let ((instance (create 'sq-table-a :field-a 19)))
      (setf (field-a instance) 77)
      (save instance))))

(test deleting
  (finishes
    (let ((instance (create 'sq-table-a :field-a 99)))
      (del instance))))

(test filtering
  (finishes
    (filter 'sq-table-a :field-a 55))
  (is (= (length (filter 'sq-table-a))
         4))
  (is (= (field-a (first (filter 'sq-table-a :field-a 1)))
         1))
  (is (= (field-a (single 'sq-table-a :field-a 1))))
  (is-true (exists 'sq-table-a :field-a 1)))

(run! 'queries)

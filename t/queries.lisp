(in-package :crane-test)

(def-suite queries
  :description "Creating, saving, and destroying objects.")
(in-suite queries)

(test creating
  (finishes
   (make-instance 'a :field-a 1)
   (make-instance 'a :field-a 2)
   (make-instance 'a :field-a 3)))

(test saving
  (finishes
    (save (make-instance 'a :field-a 42))))

(test deleting
  (finishes
    (let ((instance (make-instance 'a :field-a 23)))
      (del instance))))

(run! 'queries)

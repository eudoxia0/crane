(in-package :crane-test.sqlite3)

(test create-table
  (finishes
   (deftable sq-table-with-time ()
     (field :type timestamp))))

(test (queries :depends-on create-table)
  (is-true
   (let* ((timestamp (local-time:today))
          (instance (create 'sq-table-with-time
                            :field timestamp))
          (found-instance
            (single 'sq-table-with-time :field timestamp)))
     (local-time:timestamp= timestamp
                            (field found-instance)))))
